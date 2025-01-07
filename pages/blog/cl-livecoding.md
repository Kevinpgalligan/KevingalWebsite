title: Graphics livecoding in Common Lisp
date: 2025-01-06
description: Developing graphics applications without restarting them.
requires: code
tags: lisp programming
publish: y

Some Lisps, like Common Lisp, have a powerful feature that tends to go underappreciated amidst all the hullabaloo around macros[^hull]: the ability to recompile your program while it's running, without restarting it. For the purposes of this post, and because it sounds cool, let's call this ability *livecoding*.

Consider the typical workflow needed to make a small change to a running videogame.

1. Stop the game.
2. Change the code.
3. Wait 5 minutes for a full recompilation.
4. Restart the game.
5. Get the game state back to where it was previously.

With livecoding, you could in theory develop an entire videogame while having it running continuously, without ever having to restart it or wait more than a split second for compilation. In Common Lisp, the workflow would be:

1. Change the code.
2. Recompile it (instantaneous, because you're recompiling a few small functions individually).
3. Continue where you left off.

Livecoding isn't unique to Lisp, but I do find that Common Lisp is particularly good at it. Lisp programs are typically written alongside a REPL (Read-Eval-Print Loop), which can be used to test small pieces of the program as it's being written. Assuming the REPL isn't crap, it should be possible to redefine an individual function and immediately see the effects of its new definition in the running program.

Another useful livecoding feature, specific to Common Lisp and enshrined in its 1994 ANSI standard, is the ability to define what happens to existing instances of a class when the definition of that class gets recompiled. So not only can the behaviour of a program be updated on the fly, but also its state.

TODO: interactive debugging, stacktrace -> fix function -> recompile it -> resume execution.

TODO: these examples:
- Common Lisp and Emacs can serve as an environment for [livecoding music performances](https://www.youtube.com/watch?v=EkYUU0UoB_0), for example.
- live debugging a spacecraft (<https://corecursive.com/lisp-in-space-with-ron-garret/?utm_source=the+new+stack&utm_medium=referral&utm_content=inline-mention&utm_campaign=tns+platform#is-lisp-worth-it>)

TODO: overview of the post -- CL, Sketch, Boids (This post will first give a light introduction to Common Lisp and a Common Lisp graphics framework called [Sketch](https://github.com/vydd/sketch).)

TODO: maybe give an earlier indication of what the post will contain. Perhaps in the header description.

### A rough sketch of Common Lisp
TODO: some info about CL. Standardised, multiple impl (SBCL compiles to machine code). Seen as a more practical language compared to Scheme.

Any Lisp program can be represented inside another Lisp program as a simple data structure, either an *atom* (like `1` or `hi`) or a *list* of atoms. Lisp macros are functions that accept other Lisp code as input, and generate new Lisp code as output. Macros can do everything that other Lisp functions can do, except they're executed when code is being loaded/compiled rather than when it's evaluated.

To give a quick demonstration, here's a function to add 2 numbers together in Common Lisp.

	:::lisp
	(defun add (x y)
      (+ x y))

And here's the same code as a Lisp data structure. Note that this looks almost exactly the same as before, except the apostrophe before the opening bracket tells the Lisp compiler not to evaluate the code, resulting in a list of: 1. atoms, like `defun` and `+`; and 2. lists, like `(x y)`. This data structure can be passed around to other functions, chopped up, and put back together again in any way you can imagine. 

	:::lisp
	'(defun add (x y)
       (+ x y))

Now, here's a macro that takes a list of Lisp forms and reverses them. If `defmacro` were swapped for `defun`, this would become a function that reverses the items in a list.

	:::lisp
	(defmacro rev (c)
	  (reverse c))

With this macro, we can generate our `add` function from before by writing it backwards:

	:::lisp
	(rev ((+ x y) (x y) add defun))

I'm not sure why you'd want to do that, but it gives you a sense of what's possible with these programmable macros.


### A rough sketch of Sketch
TODO

### Livecoding Boids
TODO gif-based walkthrough

[^hull]: Despite claims to the contrary, Lisp won't give you profound insight into the fundamental nature of the universe, or make you 100 times more productive than other programmers.
