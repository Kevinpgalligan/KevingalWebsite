title: Graphics livecoding in Common Lisp
date: 2025-01-06
description: Developing a graphics application (Boids) without restarting it.
requires: code
tags: lisp programming

Some Lisps, like Common Lisp, have a powerful feature that tends to go underappreciated amidst all the hullabaloo about macros: the ability to recompile your program while it's running, without restarting it. For the purposes of this post, and because it sounds cool, let's call this ability *livecoding*[^livecod].

Entering this strange land where the programs never stop, we'll first take a brief tour of Common Lisp and one of its graphics frameworks, Sketch, before ultimately walking through a livecoded implementation of the [Boids algorithm](https://en.wikipedia.org/wiki/Boids) for simulating animal flocking behaviour.

<figure>
<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='img/cl-livecoding/boids-sample.mp4') }}" type="video/mp4">
</video> 
<figcaption>Boids!</figcaption>
</figure>

### "Wait, what's this livecoding thing again?"
Consider the typical workflow needed to make a small change to some long-running application, like a videogame.

1. Stop the application.
2. Change the code.
3. (If a compiled language) Wait N time units for a full recompilation.
4. Start the application again.
5. Interact with the application to get it back to its previous state.
6. Carry on.

In a livecoding environment, the application is never stopped, which immediately eliminates steps 1, 4 and 5. Instead, small code changes are immediately reflected in the running program. Step 3 is often instantaneous because only the changed parts of the program must be recompiled. In theory, then, you can develop an entire application while it continuously runs in the background, without ever waiting for code to recompile. This makes the development process more fluid, with minimal downtime.

In Common Lisp, the workflow would look something like this:

1. Make a small change to a single function.
2. Recompile the function (instantaneous).
3. Carry on.

It's possible to emulate livecoding in languages that don't support it. For example, a scripting language can be embedded inside the application, or the application can be made to restart itself whenever it detects a change to its source code. However, these approaches have obvious limitations compared to having first-class support in the language itself.

Some examples of livecoding in action:

* Common Lisp and Emacs have been used as an environment for [live musical performance](https://www.youtube.com/watch?v=EkYUU0UoB_0).
* Lisp programs have been debugged remotely while running in [deep space](https://corecursive.com/lisp-in-space-with-ron-garret/).
* Later in the post, we'll step through that livecoded implementation of Boids I was talking about.

### A rough sketch of Common Lisp
Since we'll be exploring the livecoding concept using Common Lisp, this section provides a whirlwind tour of the language. If you're already a Common Lisp pro or just want to wing it, feel free to skip ahead.

Common Lisp is the large, uncouth, pragmatic member of the Lisp family. It's often contrasted with its smaller, supposedly more elegant sibling, Scheme. Its ANSI specification dates back to 1994. There are multiple implementations of this standard: SBCL compiles to machine code, ECL transpiles to C, Clasp compiles to LLVM IR, and ABCL targets the JVM, to name a few. SBCL is the most performant and widely-used implementation. It tends to be about as fast as Java in benchmarks.

Here's a Common Lisp function to add together two numbers.

	:::lisp
	(defun add (x y)
      (+ x y))

And here's how you'd invoke it in the REPL:

	:::lisp
	CL-USER> (add 1 2)
    3

Some notes to accompany our first function:

* All Lisp code is in the form of *lists* and *atoms*.
* The lists are delimited by `(` and `)`, hence why Lisp is known to some as "that parentheses language"[^pars]. More specifically, these are *linked* lists, i.e. a sequence of cells where each cell contains a pointer to the next one.
* Atoms are all the things that aren't lists, like numbers and strings and symbols.
* The simplicity of the syntax is what makes Lisp macros so powerful! But that's a topic for another day... or Appendix A, below.
* `defun` is a *symbol*, a data structure for representing the names of things. The name "defun" stands for "define function".
* `add` is another symbol, representing the function name.
* `x` and `y` are the function parameters. Also symbols.
* `(+ x y)` calls the function `+`, with `x` and `y` as arguments. There's no distinction between "operators" and "functions", as you might expect from other languages. Instead, when a list is evaluated in Lisp, it's treated as a function call where the first element of the list is the function, and the remaining elements are the arguments.

Local variables are usually introduced using the `let` form. The last value in the body of the `let` is returned as its result.

	:::lisp
	(let ((x 1)
          (y 2))
      (+ x y)) ; returns 3

Global variables, if needed, can be defined using `defvar` or `defparameter`. To avoid clashing with local variables, their names are, by convention, given "earmuffs" (surrounded by asterisks).

	:::lisp
	(defparameter *global-var-bad* 42)

There's a fairly sophisticated object system, which we don't have to worry too much about today, but I'll just mention it for completeness.

	:::lisp
	(defclass foo (some-superclass)
      ((x :initarg :x :initform 0 :accessor x)
       (y :initarg :y :initform 3))
    
    ;; Make an instance of the class.
    (let ((blah (make-instance 'foo :x 5)))
      (+ (x blah) 1)) ; returns 6

And there are generic functions that dispatch on the types of the arguments they receive.

	:::lisp
    (defgeneric flerb (x))
    (defmethod flerb ((f foo))
      (+ (x f) 1))
    (flerb (make-instance 'foo :x 2)) ; returns 3

Lastly, here are two ways to calculate the sum of a list, one using a loop and the other using a reduction. This demonstrates that Common Lisp is really a multi-paradigm language. It supports looping and mutating state and all the joys/horrors of imperative programming, and also a more functional style!

	:::lisp
    (let ((sum 0))
	  (loop for x in (list 1 2 3)
            do (setf sum (+ sum x)))
      sum)

    (reduce #'+ (list 1 2 3))

A big selling-point of Lisp is its macros, but I've expelled any discussion of those to Appendix A below.

### A rough sketch of Sketch
[Sketch](https://github.com/vydd/sketch) is the graphics framework we'll be developing Boids in. It's based on the Processing API, so if you've ever used Processing or p5js, then this should be very familiar to you.

The main interface to Sketch is the `defsketch` macro. The code below generates a "sketch" called `my-sketch`.

	:::lisp
	(defsketch my-sketch
        ((width 200)
         (height 200)
         (n 5))
      [...drawing code...])

After the name of the sketch comes a list of bindings that define its state and configuration; here, the configuration parameters `width` and `height`, which determine the dimensions of the window, are set to `200`, while `n` is another state parameter of the sketch.

Then comes the drawing code. This gets run in a loop while the sketch is running. For example, we could swap out `[...drawing code...]` for the following snippet, and on every frame we would see 5 red circles, each of radius 10, drawn on a black background in random positions.

	:::lisp
    (background +black+)
    (loop repeat n
          do (with-pen (make-pen :fill +red+)
               (circle (random width) (random height) 10)))

The all-powerful `loop` macro is used to repeatedly draw circles. `with-pen` sets the "pen" that determines the appearance (fill colour, stroke colour, ...) of whatever gets drawn inside the body of `with-pen`.

Here's all the code together:

	:::lisp
	(defsketch my-sketch
        ((width 200)
         (height 200)
         (n 5))
      (background +black+)
      (loop repeat n
	        do (with-pen (make-pen :fill +red+)
	             (circle (random width) (random height) 10))))

Finally, to run the sketch, we execute `(run-sketch 'my-sketch)` from the REPL.

<figure>
<img src="{{ url_for('static', filename='img/cl-livecoding/sketch-sample.gif') }}"
     alt="The result: red circles flashing on a black background."
     class="centered">
<figcaption>Art.</figcaption>
</figure>

That's all we need to know about Sketch for now!

### Livecoding Boids
TODO:

* Go through recording, extract clips somehow.

<https://vergenet.net/~conrad/boids/pseudocode.html>

### Appendix A: Macros in Lisp
Any Lisp program can be represented inside another Lisp program as a simple data structure, either an *atom* (like `1` or `hi`) or a *list* of atoms (like `(1 (2 x))`). Lisp macros are functions that accept other Lisp code as input (in the form of an atom or list), and generate new Lisp code as output. Macros can do everything that other Lisp functions can do, except they're executed when code is being loaded/compiled rather than when it's evaluated.

To give a quick demonstration, here's a function to add 2 numbers together in Common Lisp.

	:::lisp
	(defun add (x y)
      (+ x y))

And here's the same code as a Lisp data structure.

	:::lisp
	'(defun add (x y)
       (+ x y))

Note that this looks almost exactly the same as before, except the apostrophe before the opening bracket tells the Lisp compiler not to evaluate the code (which would define the "add" function", but to instead just return the given list.

The resulting list, whose first element is the symbol `defun`, contains: 1. atoms, like `defun` and `+`; and 2. nested lists, like `(x y)`. This data structure can be passed around to other functions, chopped up, and put back together again in any way imaginable. 

Now, here's a macro that takes a list of Lisp forms and reverses them. If `defmacro` were swapped for `defun`, this would become a function that reverses the items in a list.

	:::lisp
	(defmacro rev (c)
	  (reverse c))

With this macro, we can generate our `add` function from before by writing it backwards:

	:::lisp
	(rev ((+ x y) (x y) add defun))

And now we can call it in the REPL.

	:::lisp
	CL-USER> (add 1 2)
    3

This particular example isn't very useful, but it gives you a sense of what's possible with these programmable macros. Macros can be used to extend the language syntax, make domain-specific languages, AND MORE!

### Graveyard
Lisp programs are typically written alongside a REPL (Read-Eval-Print Loop), which can be used to test small pieces of the program as it's being written. Assuming the REPL isn't crap, it should be possible to redefine an individual function and immediately see the effects of its new definition in the running program.

Another useful livecoding feature, specific to Common Lisp and enshrined in its 1994 ANSI standard, is the ability to define what happens to existing instances of a class when the definition of that class gets recompiled. So not only can the behaviour of a program be updated on the fly, but also its state.

Common Lisp also does interactive debugging really well. Inevitably, 
TODO: interactive debugging, stacktrace -> fix function -> recompile it -> resume execution.


[^livecod]: See the [Wiki page](https://en.wikipedia.org/wiki/Live_coding), and also [interactive programming](https://en.wikipedia.org/wiki/Interactive_programming).
[^pars]: Many people are scared by the abundance of parentheses in Lisp code, but experienced Lispers barely even notice the parentheses. Indentation tells you what you need to know about the code structure, while code editors keep the parentheses balanced, so the parentheses are really only there for the compiler's sake. If you ever find yourself counting parentheses, you're probably doing something wrong... unless you enjoy that sort of thing.
