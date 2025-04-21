title: Graphics livecoding in Common Lisp
date: 2025-01-06
description: Developing a Boids program from scratch without restarting it.
requires: code
tags: lisp programming artsy

Some Lisps, like Common Lisp, have a powerful feature that tends to go underappreciated amidst all the talk about macros: the ability to recompile your program while it's running, without restarting it. For the purposes of this post, and because it sounds cool, let's call this ability *livecoding*[^livecod].

Entering this strange land where the programs never stop, we'll first take a brief tour of Common Lisp and one of its graphics frameworks, Sketch, before ultimately walking through a livecoded implementation of the [Boids algorithm](https://en.wikipedia.org/wiki/Boids) for simulating animal flocking behaviour.

<figure>
<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='img/cl-livecoding/boids-sample.mp4') }}" type="video/mp4">
</video> 
<figcaption>Boids!</figcaption>
</figure>

### "Wait, what exactly is this livecoding thing?"
Consider the typical workflow needed to modify a long-running application like a videogame.

1. Stop the application.
2. Change the code.
3. (If a compiled language) Wait N time units for a full recompilation.
4. Start the application again.
5. Fiddle with the application to get it back to its previous state.
6. Carry on.

In a livecoding environment, the application is never stopped, which immediately eliminates steps 1, 4 and 5. Instead, small code changes are immediately reflected in the running program. Step 3 is often instantaneous because only the changed parts of the program must be recompiled. In theory, then, you can develop an entire application while it continuously runs in the background, without ever waiting for code to recompile. This makes the development process more fluid and interactive, with minimal downtime.

In Common Lisp, the workflow would look something like this:

1. Make a small change to a single function.
2. Recompile the function (instantaneous).
3. Carry on.

For an example of this workflow in action, check out Common Lisp and Emacs being used as an environment for [live musical performance](https://www.youtube.com/watch?v=EkYUU0UoB_0). You can hear about a Lisp program being debugged remotely while running in [deep space](https://corecursive.com/lisp-in-space-with-ron-garret/). Livecoding (or hot reloading, or whatever you like to call it) is also available in other languages, like Erlang.

### A rough sketch of Sketch
Before jumping into Boids, let's take a brief look at [Sketch](https://github.com/vydd/sketch), our Common Lisp graphics framework of choice. If you've ever used Processing or p5js, then the code should look familiar, but today we're more concerned with the big ideas than with understanding the minutiae of the code.

The main interface to Sketch is the `defsketch` macro. The code below defines a "sketch" called `my-sketch`.

	:::lisp
	(defsketch my-sketch
        ((width 200)
         (height 200)
         (n 5))
      ;; ...drawing code here...
	  )

After the name of the sketch comes a list of bindings that define its state and configuration. Here, the configuration parameters `width` and `height`, which determine the dimensions of the window, are set to `200`, while `n` is an attribute we've added for our own use.

Then comes the drawing code. This gets run in a loop while the sketch is running, once per frame. The following snippet draws 5 red circles on a black background, each of radius 10 and in random positions.

	:::lisp
    (background +black+)
    (loop repeat n
          do (with-pen (make-pen :fill +red+)
               (circle (random width) (random height) 10)))

After painting the background black, the all-powerful `loop` macro is used to draw `n` circles. The `with-pen` macro (defined by Sketch) configures drawing properties like fill colour, stroke width and stroke colour. It takes a "pen" object as an argument.

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

Finally, to run the sketch, we compile our code and execute `(run-sketch 'my-sketch)` from the REPL, resulting in...

<figure>
<img src="{{ url_for('static', filename='img/cl-livecoding/sketch-sample.gif') }}"
     alt="The result: red circles flashing on a black background."
     class="centered">
<figcaption>...art.</figcaption>
</figure>

That's all we need to know about Sketch for now!

### Livecoding Boids
[Boids](https://en.wikipedia.org/wiki/Boids) is an algorithm from 1986 for simulating flocks of birds. In its essence, it consists of applying 3 forces to the simulated birds. Quoting Wikipedia, these are:

> * separation: steer to avoid crowding local flockmates
> * alignment: steer towards the average heading of local flockmates
> * cohesion: steer to move towards the average position (center of mass) of local flockmates

How can we implement this ourselves? First, we need a canvas to draw on!

    :::lisp
	(defsketch boids
        ((width 400)
         (height 400)
         (restart-on-change nil))
	  (background (gray-255 230))

The only mysterious thing in this code is the `restart-on-change` parameter, which is available in my [fork](https://github.com/Kevinpgalligan/sketch) of Sketch. When its value is `nil` (false), the sketch's state - like the boid positions - won't be reset when we recompile our code.

Compiling in Emacs (with the Ctrl-C Ctrl-C shortcut) and executing `(run-sketch 'boids)` at the REPL gives us... 🥁... a gray background. Wonderful.

<figure>
<img src="{{ url_for('static', filename='img/cl-livecoding/boids-1-canvas.png') }}"
     alt="A light-gray background."
     class="centered">
</figure>

Note: all going well, this modest window will remain open throughout the entire development lifecycle.

Now let's create some boids to populate our world. We add a `boid` class to store their position and velocity, as well as a convenience function `make-boid` to create a boid from x & y co-ordinates. These rely on a hopefully self-explanatory implementation of 2d vectors, which are created using the `vec2` function.

	:::lisp
	(defclass boid ()
	  ((pos :initarg :pos :accessor pos)
	   (velocity :initarg :velocity
				 :initform (vec2 0 0)
				 :accessor velocity)))
	
	(defun make-boid (x y)
	  (make-instance 'boid :pos (vec2 x y)))

To the sketch, we add 20 boids in random positions, and we pass them to the `draw-boids` function in the drawing loop.

	:::lisp hl_lines="5 6 8"
	(defsketch boids
		((width 400)
		 (height 400)
		 (restart-on-change nil)
		 (boids (loop repeat 20
					  collect (make-boid (random width) (random height)))))
	  (background (gray-255 230))
	  (draw-boids boids))

If we then recompile defsketch (with Ctrl-C Ctrl-C)... 

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-2-missing-draw-compile.mp4') }}" type="video/mp4">
</video> 

...we get an error! Woops. 

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-2-missing-draw.mp4') }}" type="video/mp4">
</video> 

But of course! We forgot to define `draw-boids`. The program doesn't crash, however, and we'll soon be able to recover from this setback.

Here's an implementation of `draw-boids`. We don't need to get into the weeds of how it works. For each boid, it does some unwieldy vector math to figure out which direction it's facing and draws a triangle pointing in that direction.

	:::lisp
	(defun draw-boids (boids)
	  (let ((boid-width 10)
			(boid-length 20))
		(loop for boid in boids
			  do (with-slots (pos velocity) boid
				   (with-pen (:fill +black+)
					 (let* ((dir (if (zerop (v-length velocity))
									 (vec2 0 -1)
									 (v-normalise velocity)))
							(p1 (v+ pos (v-rescale (/ boid-length 2) dir)))
							(p2 (v+ pos
									(v-rescale (- (/ boid-length 2)) dir)
									(v-rescale (/ boid-width 2)
											   (perpendicular-anticlockwise dir))))
							(p3 (v+ pos
									(v-rescale (- (/ boid-length 2)) dir)
									(v-rescale (/ boid-width 2)
											   (perpendicular-clockwise dir)))))
					   (polygon (vx p1) (vy p1)
								(vx p2) (vy p2)
								(vx p3) (vy p3))))))))

As soon as we compile `draw-boids`, the error screen disappears and our lovely boids are drawn into place. And we didn't have to restart the program to fix it!

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-3-define-draw.mp4') }}" type="video/mp4">
</video> 

This capability - fixing errors on-the-fly - isn't specific to Sketch. When a Common Lisp program reaches an error condition, the user is presented with a selection of "restarts". TODO: Exit program; retry; swap in value for missing variable; fix bug and start from specific stack frame; possible for the program to offer custom restarts & intercept conditions.

Next steps (keep in mind that I might need to retroactively add :tweakable t):

* Add update-positions and stubs for rules
* Implement rules 1-by-1.

And: ffmpeg -i input.mp4 -ss 2 -to 4 -async 1 cut.mp4


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

### A rough sketch of Common Lisp
Since we'll be exploring the livecoding concept using Common Lisp, this section provides a whirlwind tour of the language. If you're already a Common Lisp afficionado or just want to wing it, feel free to skip ahead.

Common Lisp is the large, uncouth, pragmatic member of the Lisp family. It's often contrasted with its smaller, supposedly more elegant sibling, Scheme. Common Lisp's ANSI specification dates back to 1994. There are multiple implementations of this standard: SBCL compiles to machine code, ECL transpiles to C, Clasp compiles to LLVM IR, and ABCL targets the JVM, to name a few. SBCL is the most performant and widely-used implementation. It tends to be about as fast as Java in benchmarks.

Here's a Common Lisp function to add together two numbers.

	:::lisp
	(defun add (x y)
      (+ x y))

And here's how you'd call that function in a REPL:

	:::lisp
	CL-USER> (add 1 2)
    3

Some notes to accompany our first function:

* All Lisp code is in the form of *lists* and *atoms*.
* The lists are delimited by `(` and `)`, hence why Lisp is known to some as "that parentheses language"[^pars].
* A list is a chain of *cons cells*. Each cons cell contains a value, and a pointer to the next cons cell.
* Atoms are all the things that aren't lists, like numbers and strings and symbols.
* The simplicity of the syntax is what makes Lisp macros so powerful! But that's a topic for another day... or Appendix A, below.
* `defun` is a *symbol*.
* A symbol is a data structure for representing the names of things. The name "defun" stands for "define function".
* `add` is another symbol, representing the name of our function.
* `x` and `y` are the function parameters. Also symbols.
* `(+ x y)` is a list consisting of the symbols `+`, `x` and `y`.
* When a list is being evaluated, the first calls the function `+`, with `x` and `y` as arguments. There's no distinction between "operators" and "functions", as you might expect from other languages. Instead, when a list is evaluated in Lisp, it's treated as a function call where the first element of the list is the function, and the remaining elements are the arguments.

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

### Graveyard
Lisp programs are typically written alongside a REPL (Read-Eval-Print Loop), which can be used to test small pieces of the program as it's being written. Assuming the REPL isn't crap, it should be possible to redefine an individual function and immediately see the effects of its new definition in the running program.

Another useful livecoding feature, specific to Common Lisp and enshrined in its 1994 ANSI standard, is the ability to define what happens to existing instances of a class when the definition of that class gets recompiled. So not only can the behaviour of a program be updated on the fly, but also its state.

Common Lisp also does interactive debugging really well. Inevitably, 
TODO: interactive debugging, stacktrace -> fix function -> recompile it -> resume execution.

It's possible to emulate livecoding in languages that don't support it. For example, a scripting language can be embedded inside the application, or the application can be made to restart itself whenever it detects a change to its source code. However, these approaches have obvious limitations compared to having first-class support in the language itself.

[^livecod]: See the [Wiki page](https://en.wikipedia.org/wiki/Live_coding), and also [interactive programming](https://en.wikipedia.org/wiki/Interactive_programming).
[^pars]: Many people are scared by the abundance of parentheses in Lisp code, but experienced Lispers barely even notice it. Indentation tells you what you need to know about the code structure, while code editors keep the parentheses balanced, so the parentheses are really only there for the compiler's sake. If you ever find yourself counting parentheses, you're probably doing something wrong... unless you enjoy that sort of thing.
