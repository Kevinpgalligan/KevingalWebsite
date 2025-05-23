title: Symbols and Packages in Common Lisp
date: 2025-05-14
description: A guide.
requires: code
tags: lisp programming

Lisp programming languages have an extremely simple syntax, where everything is either a list or an atom. The most common type of atom is the **symbol**, which is used to name things like variables and functions. In the Common Lisp code below, `defun`, `pie`, `let`, `+`, `x` and `y` are all symbols.

	:::lisp
	(defun pie (x)
      (let ((y 1))
        (+ x y)))

When I started learning Common Lisp, I remember being confused by this superficially simple data structure, as well as the related concept of packages. This article is an attempt to explain symbols and packages to my past self. It's vaguely FAQ-like, so feel free to skip around to different parts. As much as possible, I'll attempt to "teach you to fish" by exploring concepts in the REPL.

<figure>

<img src="{{ url_for('static', filename='img/cl-packages/cl-user.png') }}"
     alt="A box labelled 'common-lisp-user', with a stick figure inside and two symbols, 'car' and 'cdr'.."
     class="centered">
<figcaption>The common-lisp-user package and some of the symbols it contains.</figcaption>
</figure>

## What is a symbol?
When it comes to definitions, it's helpful to consult Common Lisp's ANSI specification, published in 1994. [Quote](https://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm):

> Symbols are used [...] to name various entities in Common Lisp, including (but not limited to) linguistic entities such as variables and functions.

In other words, a symbol is a data structure for identifying things. It's important to emphasise that **symbols are not strings**. Each symbol has an associated name, which is represented as a string, but the symbol itself is not a string. I point this out because strings are the closest primitive data type to symbols in most other programming languages.

## What is to be done... with symbols?
If we enter just `x` at the REPL, it will be evaluated and the variable value associated with the symbol `x` (if such a value exists) will be the result. To get at the symbol itself, we need to `quote` it to avoid evaluating it, like `(quote x)`, or `'x` for short.

With this knowledge, let's test the statement above, that a symbol is NOT the same as its name. We need the `symbol-name` function to fetch the name from a symbol data structure.

	:::lisp
    >>> (type-of 'x)
	SYMBOL
	>>> (symbol-name 'x)
    "X"
	>>> (equalp 'x (symbol-name 'x))
	NIL     ; not equal!

At read time, when the raw Lisp code is slurped in and parsed, each occurrence of a name will be mapped to a unique symbol. And so, in this example, we're comparing the same symbol to itself.

	:::lisp
	>>> (equalp 'x 'x)
    T    ; equal

Common Lisp follows the [Lisp-2 model](https://stackoverflow.com/questions/4578574/what-is-the-difference-between-lisp-1-and-lisp-2), so functions and variables have separate namespaces. As such, a symbol can simultaneously identify both a function, which we fetch with `symbol-function`, and a variable, which we fetch with `symbol-value`. In the following example, the symbol `porridge` refers to both a function and a variable.

	:::lisp
    ;; Define function.
	>>> (defun porridge ()
          (+ 1 1))

    ;; Define variable.
	>>> (defparameter porridge 10)

	;; Symbol now refers to both a function and a variable.
	;; We try getting the value, getting the function, evaluating
	;; the symbol, and using it in a function call.
    >>> (list (symbol-value 'porridge)
              (symbol-function 'porridge)
              porridge
              (porridge)))
    (10 #<FUNCTION PORRIDGE> 10 2) ; <--- result

Each symbol also has an associated [property list](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list), accessed with `symbol-plist`. There's also `symbol-package`, which we'll discuss later.

## Why is the symbol name in all-caps?
Good catch: we entered the symbol `'x`, but the printed symbol name was `"X"`. By default, all symbol names are converted to upper case by the so-called Lisp Reader. This can be controlled with a parameter called [readtable-case](https://www.lispworks.com/documentation/HyperSpec/Body/23_ab.htm), which, if you really want to preserve your precious little capitalisation, can be set to `:preserve`.

Honestly, I think this is one of the language's biggest warts. Under the default behaviour, if you're trying to implement a formula that contains - *gasp* - both `n` AND `N`, you're reduced to mutating one of the names, like a wretched internet user trying to meet password requirements by going from "password" to "Password1!".

There is, however, another way to preserve case in symbol names. Wrap the name in `|`s, like `'|x|`.

	:::lisp
	>>> (symbol-name '|hi|)
	"hi"

## The secrets of the defun
Remember that symbols have a `symbol-function` property? We can overwrite the value of this property with `setf`. That's essentially what `defun` is doing behind the scenes.

	:::lisp
	>>> (setf (symbol-function 'porridge)
              (lambda (x) (+ x 1)))
	...
	>>> (porridge 1)
	2

This enables us to reimplement `defun` as a 2-line macro, a somewhat cool fact that was first demonstrated to me in the book *On Lisp*. Imagine being able to define your own version of `def` in Python!

	:::lisp
	(defmacro mydefun (name parameters &body body)
	  `(setf (symbol-function ',name) (lambda ,parameters ,@body)))

## You idiot, Kevin! symbol-value doesn't work like you said!!
Based on what we've covered already, it'd be reasonable to expect the following code to work.

	:::lisp
	(let ((x 1))
      (symbol-value 'x))

But instead this raises an error about `x` being unbound.

Here's why: `let` creates a [*lexical binding*](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lexical_binding) for the symbol `x`. Within the body of that `let`, `x` is lexically bound to the value `1`, but that doesn't affect the *global* value of `x`, which is what `symbol-value` tries to retrieve.

During evaluation, lexical bindings take precedence over global ones, so just evaluate the symbol to get its lexically bounded value.

	:::lisp
	>>> (defparameter x 1)
	>>> (let ((x 5))
		  x)
    5

"Wow," you might be thinking, "Doesn't this make it easy to accidentally shadow a global variable with a local variable?". Well, yes. That's why, similar to most other programming languages, Common Lisp has a naming convention for global variables. They're given "earmuffs", so `x` becomes `*x*`.

## What is a package?
Symbols, as you can see, are quite simple data structures. Where it gets a bit trickier is in their interaction with **packages**. Again [quoting](https://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm) the spec:

> A package is a namespace that maps symbol names to symbols [...]

Packages allow *your* implementation of `fizzbuzz` to live alongside *my* `fizzbuzz`. If our packages are named `apple` and `banana`, and they both export a symbol called `fizzbuzz` for external use, then those symbols are referenced using `apple:fizzbuzz` and `banana:fizzbuzz`.

Once a package has been created inside a running Lisp process, it's visible everywhere. There's no concept of "importing" one package into another. Any package can reference any other package without anything like an "import".

## How do symbols get added to packages?
When the Lisp Reader slurps in some Lisp code, all of the newly-created symbols from that code are "interned" in (added to) the currently active package, designated by a global variable called [\*package\*](https://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm#STpackageST). The macro `(in-package <package-name>)` changes the currently active package.

Programs always start in the COMMON-LISP-USER package, which also goes by the nickname CL-USER. This package has all the symbols you know and love from the ANSI spec: `car`, `cdr`, `loop`, `defun`, etc. We'll explore CL-USER in more detail later on.

	:::lisp
	CL-USER> *package*
	#<PACKAGE "COMMON-LISP-USER">

("CL-USER>" is the command prompt from the SLIME REPL, indicating that CL-USER is the currently active package).

It's also possible to manually add symbols to a package with the `intern` function. Here we see that `intern` does NOT modify the case of the symbol name - that particular wart is inflicted upon us by the Lisp Reader.

	:::lisp
	CL-USER> (intern "hi")
	|hi|

If a symbol of the given name exists already, then that symbol is returned rather than creating a new one.

	:::lisp
	CL-USER> (eq '|hi| (intern "hi"))
	T

By default, `intern` adds the symbol to the currently active package, but we can specify a different package. Let's say there's another package called `BEANS`. Then...

	:::lisp
	;; Add symbol named TOAST to the BEANS package.
	CL-USER> (intern "TOAST" (find-package "BEANS"))
	BEANS::TOAST

	;; Switch to the BEANS package.
	CL-USER> (in-package beans)
	#<COMMON-LISP:PACKAGE "BEANS">

	;; Now we can access the TOAST symbol we added earlier.
	BEANS> (find-symbol "TOAST")
	TOAST

(We've used `find-package` and `find-symbol` here, which are hopefully self-explanatory. We can't evaluate `(quote toast)` to verify that the symbol has been created, because "TOAST" will be interned by the Lisp Reader in the process).

## What's in CL-USER?
It can be nice to explore this sort of thing in the SLIME inspector. If you happen to have a SLIME REPL open in Emacs, enter `(find-package "COMMON-LISP-USER")` or `(find-package "CL-USER")`.

	:::lisp
	CL-USER> (find-package "COMMON-LISP-USER")
	#<PACKAGE "COMMON-LISP-USER">

Now right click on the output, and click "Inspect", and you should see something like the following.

	#<PACKAGE {1000013983}>
	--------------------
	Name: "COMMON-LISP-USER"
	Nicknames: "CL-USER"
	Documentation:
	public: the default package for user code and data
	Use list: COMMON-LISP, SB-ALIEN, SB-DEBUG, SB-EXT, SB-GRAY, SB-PROFILE
	Used by list: [nothing here]
	4 present symbols.
	0 external symbols.
	4 internal symbols.
	1329 inherited symbols.
	0 shadowed symbols.

We see the package's name, its nicknames, and its documentation. Then there's the "Use list", containing all the packages that COMMON-LISP-USER inherits symbols from. My CL-USER package inherits from COMMON-LISP, as well as various packages defined by the SBCL implementation of Common Lisp, which I happen to be using. The symbols from the ANSI spec live in the COMMON-LISP package, which we can confirm with `symbol-package`:

	:::lisp
	(symbol-package 'car)
	#<PACKAGE "COMMON-LISP">

(`symbol-package` tells us the "home package" of a symbol; that is, where it was initially intern'd).

Next in the inspector output, we see that CL-USER has 4 "present" symbols, which are symbols whose home package is CL-USER. We can check which symbols those are as follows:

	:::lisp
	CL-USER> (do-symbols (s)
               (when (eq (symbol-package s) (find-package "CL-USER"))
                 (format t "~a~%" s)))
	BEANS
	S
	hi
	QUICKLISP-INIT
	HI

(In the course of evaluating this, the Lisp Reader intern'd another symbol, `S`, into CL-USER, and so now there are 5).

Back to the inspector output. CL-USER doesn't export any symbols, so all 4 (now 5) of the present symbols are "internal symbols". Then there are 1329 inherited symbols from COMMON-LISP and the SBCL packages. We don't shadow any of those symbols. Shadowing allows you, for example, to define your own implementation of `car` or `+`, but it's usually considered a bad idea.

## Playing around with some sample packages
Let's define a package called `beatles`. The most common way to do this is via `defpackage`, which accepts a number of optional parameters.

	:::lisp
	(defpackage beatles
      (:use :cl)
	  (:export :play))

The parameters we've used here:

* `(:use :cl)` -- This makes BEATLES inherit all the exported symbols from the COMMON-LISP package (nicknamed CL). You'll almost always want to do this, because otherwise you'll have to prefix all the spec-defined symbols with `cl:`.
* `(:export :play)` -- Exporting a symbol makes it visible outside the package. We're going to define a `play` function inside the BEATLES package and export it.

Why do many of these symbols have a colon as a prefix? This is a special syntax for referencing "keyword symbols", which are symbols in the [KEYWORD package](https://www.lispworks.com/documentation/HyperSpec/Body/11_abc.htm). One of the common uses of keyword symbols is to avoid flooding a package namespace with symbols that will never be used again. If we wrote this defpackage form without keyword symbols, then symbols called `cl` and `play` would be pointlessy intern'd into the CL-USER package (or whichever package is active). In fact, as it stands, we're pointlessly interning the `beatles` symbol. We could avoid this by writing `(defpackage :beatles ...)` instead. The same applies for calls to `find-package`, as `(find-package :beatles)` works just as well as `(find-package 'beatles)` or `(find-package "BEATLES")`.

Anyway, let's now define some functions in the BEATLES package. For one of these functions, its identifying symbol, `play`, will be exported, meaning it's visible outside the BEATLES package.

	:::lisp
	(in-package beatles) ; or (in-package :beatles)
    
	(defun play ()
	  (format t "badum tss~%"))
    
	(defun sing ()
      (format t "she loves you yeah yeah yeah~%"))

Now let's define another package, STONES, which inherits symbols from the BEATLES package as well as CL.

	:::lisp
	(defpackage :stones
	  (:use :cl :beatles))

	(in-package :stones)
    
	(defun sing ()
      (format t "something something paint it black~%"))

Here's a diagram of all these packages. The stickperson - let's call them Frankie - represents the active package.

<img src="{{ url_for('static', filename='img/cl-packages/example.png') }}"
     alt="3 boxes labelled 'common-lisp-user', 'beatles' and 'stones'. The first contains a stick figure and a few symbols: 'car, 'cdr, etc. The 'beatles' box contains 'play, 'sing, 'car (from CL), etc. The 'stones box contains 'play (from 'beatles'), 'sing, 'car (from CL), etc."
     class="centered">

We start off in CL-USER. The BEATLES package exports the `play` symbol, so we can call the function it identifies, but not `sing`.

	:::lisp
	CL-USER> (beatles:play)
	badum tss
	NIL

	CL-USER> (beatles:sing)
	; (Error: The symbol SING is not external in the BEATLES package.)

Of course, if we REALLY wanted to access an internal symbol from a package, we could use the double colon syntax.

	:::lisp
	CL-USER> (beatles::sing)
	she loves you yeah yeah yeah
	NIL

The STONES package doesn't export any symbols, so we can't access its implementation of `sing`. Let's try making it the active package.

	:::lisp
	CL-USER> (in-package :stones)
	#<PACKAGE "STONES">

	STONES> *package*
	#<PACKAGE "STONES">

Since STONES inherits symbols from BEATLES, we can call the BEATLES implementation of `play`. Also check to make sure it's really coming from BEATLES.

	:::lisp
	STONES> (play)
	badum tss
	NIL

	STONES> (symbol-package 'play)
	#<PACKAGE "BEATLES">

STONES didn't inherit the `sing` symbol from BEATLES, but instead defined its own implementation of `sing`.

	:::lisp
	STONES> (sing)
	something something paint it black
	NIL

	STONES> (symbol-package 'sing)
	#<PACKAGE "STONES">

Finally, I want to drive home the point that the same symbol data structure is used for `play` in both BEATLES and STONES, but different symbols are used for `sing`. This is because `play` only got interned inside the BEATLES package, and the resulting symbol was inherited by STONES. `sing`, on the other hand, was interned separately into both packages, creating 2 distinct symbols as a result.

	:::lisp
	STONES> (eq (find-symbol "PLAY")
            (find-symbol "PLAY" (find-package :beatles)))
	T

	STONES> (eq (find-symbol "SING")
				(find-symbol "SING" (find-package :beatles)))
	NIL

## What's the deal with the KEYWORD package?
We've already touched briefly on the KEYWORD package. Besides avoiding namespace pollution, what is it good for?

[Quoting the spec](https://www.lispworks.com/documentation/HyperSpec/Body/11_abc.htm):

> The KEYWORD package contains symbols, called keywords, that are typically used as special markers in programs and their associated data expressions. 

Yes, like how they're used as the labels for function keyword arguments! Another insightful quote from the spec...

> Symbol tokens that start with a package marker are parsed by the Lisp reader as symbols in the KEYWORD package; see Section 2.3.4 (Symbols as Tokens). This makes it notationally convenient to use keywords when communicating between programs in different packages. For example, the mechanism for passing keyword parameters in a call uses keywords to name the corresponding arguments [...]

Here's an example of how keywords are convenient for "communicating between programs in different packages". Let's say the BEATLES package exports a `favourite` macro that generates a print statement based on the code it receives.

	:::lisp
	(in-package :beatles)
	(defmacro favourite (beatle)
      (if (eq beatle :john)
          '(format t "correct~%")
		  '(format t "wrong~%")))

If we now call this macro from the STONES package...

	:::lisp
	STONES> (beatles:favourite :john)
	correct

	STONES> (beatles:favourite :ringo)
	wrong

...it behaves as we'd expect. If we'd instead implemented `favourite` using the comparison `(eq beatle 'john)`, then `(beatles:favourite 'john)` and `(beatles:favourite 'ringo)` would, perhaps surprisingly, *both* print "wrong", because the `john` symbol we'd be reading into the STONES package would be different to the `john` symbol in the BEATLES package.

More usage advice from the spec:

>  It is generally best to confine the use of keywords to situations in which there are a finitely enumerable set of names to be selected between. For example, if there were two states of a light switch, they might be called :on and :off. 
>
>  In situations where the set of names is not finitely enumerable (i.e., where name conflicts might arise) it is frequently best to use symbols in some package other than KEYWORD so that conflicts will be naturally avoided. For example, it is generally not wise for a program to use a keyword[1] as a property indicator, since if there were ever another program that did the same thing, each would clobber the other's data. 

## Can a symbol be homeless?
We've seen how symbols have a home package, which is the package into which they were originally interned. `(symbol-package 'sym)` yields this package. Here are some ways for a symbol to be homeless, such that `(symbol-package 'sym)` will return `NIL`.

1) Manually create a symbol data structure: `(make-symbol "NAME-HERE")`.

	:::lisp
	CL-USER> (symbol-package (make-symbol "fuck"))
	NIL

2) Syntax sugar for that: `'#:name-here`. Note that `(eq '#:hi '#:hi)` returns `NIL`.

3) `unintern` a symbol to remove it from a package:


    :::lisp
    CL-USER> (find-symbol "S")
	S
	:INTERNAL
	CL-USER> (unintern 's)
	T
	CL-USER> (find-symbol "S")
	NIL
	NIL

## Symbols and macros
* symbols get read by Lisp Reader in one package, they're now a data structure in memory, macro doesn't care where the symbols come from
* gensyms.
* (don't bother trying to manually set *package* in a macro, since it takes effect at readtime... this sidenote can be another section)

## When I compile a single function, how does SLIME know which package it's in?
If you're an Emacs user and you've compiled a single Common Lisp form, like a function, using the `C-c C-c` shortcut, then you may have wondered how SLIME knows which package should be active when the Lisp Reader slurps in that form. For example, you might have the following code in one buffer:

	:::lisp
	(in-package foo)
	(defun bar ()
	  (+ 1 1))

In your REPL, the currently active package is CL-USER. So when you compile the `bar` function, why does it get added to the FOO package rather than CL-USER?

This is an implementation detail of SLIME more than anything, but I remember being confused by it, so let's indulge ourselves with a brief diversion. First, let's see what ELisp function `C-c C-c` is bound to. Entering the help command for keyboard shortcuts, `C-h k`, and then `C-c C-c`, we're told that this shortcut is bound to the function `slime-compile-defun`.

This function is defined in `slime.el`; following the link there and searching for "in-package", we find the following comment:

> We have the concept of the "current Lisp package". RPC requests always say what package the user is making them from and the Lisp side binds that package to \*BUFFER-PACKAGE\* to use as it sees fit. The current package is defined as the buffer-local value of 'slime-buffer-package' if set, and otherwise the package named by the nearest IN-PACKAGE as found by text search (cl-first backwards, then forwards).

And there's our answer: unless `slime-buffer-package` is set, it first searches backwards and then forwards for the nearest IN-PACKAGE form. Not mentioned is that if no IN-PACKAGE form is found, then the code is compiled in the currently active package, whether that's CL-USER or something else.

For curiosity's sake, here's the ELisp function used to search for IN-PACKAGE.

	:::lisp
	(defun slime-search-buffer-package ()
	  (let ((case-fold-search t)
			(regexp (concat "^[ \t]*(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
							"\\([^)]+\\)[ \t]*)")))
		(save-excursion
		  (when (or (re-search-backward regexp nil t)
					(re-search-forward regexp nil t))
			(match-string-no-properties 2)))))

## Conclusions and further reading
Missing stuff: boundp; fboundp?; makunbound; fmakunbound; that thing where you try to use a symbol, error, but then it gets interned, and when you try to import the symbol from another package, there's a clash; mention inspiration from the PDF guide; table of contents

- <https://lisp-docs.github.io/docs/tutorial/projects/guide_to_packages>
- "Packages in Common Lisp, a tutorial" - link to PDF, <https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/Packaging.pdf>
