title: An Earnest Guide to Symbols in Common Lisp
date: 2025-06-04
description: ...with an emphasis on exploratory programming and examples.
requires: code
tags: lisp programming
publish: y

Lisp programming languages have an extremely simple syntax, where everything is either a list or an atom. The most common type of atom is the **symbol**, which is used to name things like variables and functions. In the Common Lisp code below, `defun`, `pie`, `let`, `+`, `x` and `y` are all symbols.

	:::lisp
	(defun pie (x)
      (let ((y 1))
        (+ x y)))

The symbol might be a simple data structure, but its interaction with other parts of Common Lisp - packages, the Lisp Reader, macros - is complicated. This article is a vaguely FAQ-like exploration of symbols and packages that will hopefully illuminate some of these tricky interactions. Worth reading before, after, or in parallel, is (PDF warning) [The Complete Idiot's Guide to Common Lisp Packages](https://flownet.com/ron/packages.pdf).

<figure>

<img src="{{ url_for('static', filename='img/cl-packages/cl-user.png') }}"
     alt="A box labelled 'common-lisp-user', with a stick figure inside and two symbols, 'car' and 'cdr'.."
     class="centered">
<figcaption>The common-lisp-user package and some of the symbols it contains.</figcaption>
</figure>

<h2>Table of Contents</h2>
[TOC]

## What is a symbol?
When it comes to definitions, it's helpful to consult Common Lisp's ANSI specification, published in 1994. [Quote](https://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm):

> Symbols are used [...] to name various entities in Common Lisp, including (but not limited to) linguistic entities such as variables and functions.

In other words, a symbol is a data structure for identifying things. It's important to emphasise that **symbols are not strings**. Each symbol has an associated name, which is represented as a string, but the symbol itself is not a string. I point this out because strings are the closest primitive data type to symbols in most other programming languages.

## What is to be done... with symbols?
If we enter just `x` at the REPL, the symbol of that name will be evaluated, with the result being the **value** referred to by that symbol.

	:::lisp
	>>> (defparameter x 1)
	[...]

	>>> x
	1

To get at the symbol itself, we need to `quote` it so that it doesn't get evaluated, like `(quote x)`, or `'x` for short. Let's use this knowledge to test the statement above, that a symbol is NOT the same as its name. We need the `symbol-name` function to fetch the name from a symbol data structure.

	:::lisp
    >>> (type-of 'x)
	SYMBOL

	>>> (symbol-name 'x)
    "X"

	>>> (equal 'x (symbol-name 'x))
	NIL     ; not equal!

At read time, when the raw Lisp code is slurped in and parsed, all occurrences of a particular name are mapped to a single, unique symbol. And so, in the next example, we're comparing the same symbol to itself.

	:::lisp
	>>> (equal 'x 'x)
    T    ; equal

Common Lisp follows the [Lisp-2 model](https://stackoverflow.com/questions/4578574/what-is-the-difference-between-lisp-1-and-lisp-2), so functions and variables have separate namespaces. As such, a symbol can simultaneously identify both a function, which we fetch with `symbol-function`, and a variable, which we fetch with `symbol-value`. In the following example, the symbol `porridge` refers to both a function and a variable.

	:::lisp
    ;; Define function.
	>>> (defun porridge ()
          (+ 1 1))
	[...]

    ;; Define variable.
	>>> (defparameter porridge 10)
	[...]

	;; Symbol now refers to both a function and a variable.
    >>> (symbol-value 'porridge)
    10
    
	>>> (symbol-function 'porridge)
	#<FUNCTION PORRIDGE>
   	
	;; Compare the above to evaluating the symbol...
    >>> porridge
    10
    
	;; ...or invoking the function it identifies.
	>>> (porridge)
    2

`(boundp 's)` and `(fboundp 's)` identify whether the symbol `s` is currently bound to a value or function, respectively. `(makunbound 's)` and `(fmakunbound 's)` remove the binding.

	:::lisp
	>>> (defparameter x 3)
	X

	>>> (boundp 'x)
	T

	>>> (fboundp 'x)
	NIL

	>>> (makunbound 'x)
	[...]

	>>> (boundp 'x)
	NIL


Each symbol also has an associated [property list](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list), accessed with `symbol-plist`, but that doesn't rear its head very often. There's also `symbol-package`, which we'll discuss later.

## Why is the symbol name in all-caps?
Good catch: we entered the symbol `'x`, but the printed symbol name was `"X"`. By default, all symbol names are converted to upper case by the so-called Lisp Reader. This can be controlled with a parameter called [readtable-case](https://www.lispworks.com/documentation/HyperSpec/Body/23_ab.htm), which, if you really want to preserve your precious little capitalisation, can be set to `:preserve`.

Honestly, I think this is one of the language's biggest warts. Under the default behaviour, if you're trying to implement a formula that contains - *gasp* - both `n` AND `N`, you're reduced to mutating one of the names, as if you were a wretched internet user trying to meet password requirements by going from "password" to "Password1!".

That said, there's another way to preserve case in symbol names. Wrap the name in `|`s, like `'|x|`.

	:::lisp
	>>> (symbol-name '|hi|)
	"hi"

    >>> (eq 'hi '|hi|)
	NIL   ; they have different names

## &lt;_%&? is a symbol
The rules for symbol names are somewhat unusual, compared to other programming languages. Case in point: `<_%&?` is a valid symbol name.

	:::lisp
	>>> (symbol-name '<_%&?)
	"<_%&?"

What are these rules? They're described in [Section 2.1](https://www.lispworks.com/documentation/HyperSpec/Body/02_a.htm) of the spec, and were admirably summarised by [ScottBurson](https://www.reddit.com/r/Common_Lisp/comments/1l3apg1/comment/mw1srxb/) on Reddit -- now reproduced here with their permission.

Any consecutive sequence of characters will be parsed as a symbol, except:

* whitespace.
* parentheses/braces/brackets.
* those that can be parsed as numbers.
* those starting with certain special characters, including `'`, `` ` ``, `,`, and `#`.
* a single period by itself (called "dot").

As exceptions to these exceptions:

* any sequence of characters surrounded by `|` is parsed as a symbol.
* the non-symbol meaning of any character (including `|`) can be suppressed by preceding it with `\`.

So here are some valid symbols: `foo`, `foo-bar`, `=`, `<_%&?`, `23-and-me`, `|(|`, and `\(`.

## The secrets of the defun
Y'know how symbols have a `symbol-function` property? We can overwrite the value of this property with `setf`. That's essentially what `defun` is doing behind the scenes.

	:::lisp
	>>> (setf (symbol-function 'porridge)
              (lambda (x) (+ x 1)))
	[...]

	>>> (porridge 1)
	2

This enables us to reimplement `defun` as a macro, a somewhat cool fact that was first demonstrated to me in the book *On Lisp*.

	:::lisp
	(defmacro mydefun (name parameters &body body)
	  `(setf (symbol-function ',name)
			 (lambda ,parameters ,@body)))

## Bindings and symbols
Based on what we've covered already, it'd be reasonable to expect the following code to work.

	:::lisp
	(let ((x 1))
      (symbol-value 'x))

But instead this raises an error about `x` being unbound.

Here's why: the `let` form creates a [lexical binding](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_b.htm#binding) between the symbol `x` and the value `1`, which persists throughout the body of the `let`. This is comparable to declaring a local variable in other languages. It doesn't affect the *global* value of `x`, which is what `symbol-value` attempts to retrieve, and hence the error.

If `x` has already been declared a dynamic (a.k.a. global) variable, however, then the binding created by `let` will shadow whatever global value `x` has. Other binding constructs within the language work the same way, e.g. a function parameter called `x` would also shadow the global variable.

	:::lisp
	>>> (defparameter x 1)
    [...]

	>>> (let ((x 5))
		  (symbol-value 'x))
    5

	>>> (symbol-value 'x)
    1

(The exact details around variables, bindings, lexical scope, dynamic scope, etc. are beyond the scope of this article).

"Wow," you might be thinking, "Doesn't this make it easy to accidentally mutate a global variable?". Well, yes. That's why, similar to most other programming languages, Common Lisp has a naming convention for global variables. They're given "earmuffs", so that `x` becomes `*x*`.

## What is a package?
It's impossible to get a full picture of how symbols work without also understanding the related concept of **packages**. These are somewhat like namespaces or "packages" in other languages, except they're an actual data structure that we can directly query and manipulate.

Again [quoting](https://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm) the spec:

> A package is a namespace that maps symbol names to symbols [...]

The main use case of packages is that they allow *your* implementation of `fizzbuzz` to live alongside *my* `fizzbuzz`. If our packages are named `apple` and `banana`, and they both export a symbol called `fizzbuzz` for external use, then those symbols are referenced using `apple:fizzbuzz` and `banana:fizzbuzz`.

Note that packages don't have any notion of variables or functions, only symbols. That's why they're sometimes described as a "bag of symbols".

Once a package has been created inside a running Lisp process, it's visible everywhere. If there's a package called `scooby`, and it exports the symbol `doo`, then that symbol can be referenced anywhere via `scooby:doo`, with no restrictions.

## How do symbols get added to a package?
When the Lisp Reader inhales some Lisp code, it has to map each symbol name to a symbol data structure. It does this by "interning" each name into the currently active package, which is designated by a global variable called [\*package\*](https://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm#STpackageST). `(intern "HI")` returns the existing symbol called "hi" if it already exists in `*package*`; otherwise, it adds a new symbol named "hi" to `*package*` and returns it. This behaviour ensures that each occurrence of a symbol name will map to the same symbol data structure.

	:::lisp
	CL-USER> (intern "hi")
	|hi|   ; note: intern doesn't capitalise names

	CL-USER> (eq '|hi| (intern "hi"))
	T      ; they're the same symbol

("CL-USER>" is the command prompt from the SLIME REPL, indicating that CL-USER is the currently active package).

Programs always start in the COMMON-LISP-USER package, which also goes by the nickname CL-USER. This package gives access to all the symbols you know and love from the ANSI spec: `car`, `cdr`, `loop`, `defun`, etc. We'll explore CL-USER in more detail later on. The macro `(in-package <package-name>)` changes the currently active package.

	:::lisp
	CL-USER> *package*
	#<PACKAGE "COMMON-LISP-USER">

	CL-USER> (in-package :derp)
	#<PACKAGE "DERP">

	DERP> *package* 
	#<PACKAGE "DERP"> ; DERP is now the active package

By default, `intern` adds the symbol to the currently active package, but we can specify a different package. Let's say there's a package called `BEANS`. Then...

	:::lisp
	;; Add symbol named TOAST to the BEANS package.
	CL-USER> (intern "TOAST" (find-package "BEANS"))
	BEANS::TOAST  ; double colon because BEANS doesn't export TOAST

	;; Switch to the BEANS package.
	CL-USER> (in-package beans)
	#<COMMON-LISP:PACKAGE "BEANS">

	;; Now we can access the TOAST symbol we added earlier.
	BEANS> (find-symbol "TOAST")
	TOAST

We've used `find-package` and `find-symbol` here, which are hopefully self-explanatory. We couldn't just evaluate `(quote toast)` to verify that the symbol exists, because in the process of reading that form, the `toast` symbol would be created and added to the active package by the Lisp Reader.

## Print-read consistency
If you're confused by the printed representation of symbols, it might be due to print-read consistency, described by The Complete Idiot's Guide as follows:

> One of the invariants that Common Lisp tries to maintain is a property called print-read consistency. This property says that if you print a symbol, and then read the resulting printed representation of that symbol, the result is the same symbol, with two caveats: 1) this does not apply to uninterned symbols, and 2) [...]

(To find out the other caveat, go read the original ;)

For example, in our demonstration of the `intern` function, the `toast` symbol was printed as "BEANS::TOAST" when we were in the CL-USER package, while within the BEANS package it was printed as "TOAST". This is because `toast` is *internal* to the BEANS package, and to reference it from outside the BEANS package, we have to use that double colon. If BEANS happened to export the `toast` symbol, we'd only need 1 colon and the printed representation from CL-USER would be "BEANS:TOAST". *Within* the BEANS package, we don't need a package prefix, hence why it's just "TOAST".

## What's in CL-USER?
It can be nice to explore this sort of question in the SLIME inspector. If you happen to have a SLIME REPL open in Emacs, enter `(find-package "COMMON-LISP-USER")` or `(find-package "CL-USER")`.

	:::lisp
	CL-USER> (find-package "COMMON-LISP-USER")
	#<PACKAGE "COMMON-LISP-USER">

Now right click on the output, click "Inspect", and you should see something like the following.

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

(`symbol-package` tells us the "home package" of a symbol; that is, where it was initially interned).

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

(In the course of evaluating this, the Lisp Reader interned another symbol, `S`, into CL-USER, and so now there are 5 present symbols).

Back to the inspector output. CL-USER doesn't export any symbols, so all 4 (now 5) of the present symbols are "internal symbols". Then there are 1329 inherited symbols from COMMON-LISP and the SBCL packages. We don't shadow any of those symbols. Shadowing allows you, for example, to define your own implementation of `car` or `+`, but it's usually considered a bad idea, since it subverts people's expectations of what those symbols refer to.

## Playing around with some sample packages
Let's define a package called `beatles`. The most common way to do this is via the `defpackage` macro, which has a number of options.

	:::lisp
	(defpackage beatles
      (:use :cl)
	  (:export :play))

The options we've used here:

* `(:use :cl)` -- This makes BEATLES inherit all the exported symbols from the COMMON-LISP package (nicknamed CL). You'll almost always want to do this, because otherwise you'll have to prefix all the spec-defined symbols with `cl:`.
* `(:export :play)` -- Exporting a symbol makes it visible outside the package. We're going to define a `play` function inside the BEATLES package and export the symbol.

Why do many of these symbols have a colon as a prefix? This is a special syntax for referencing "keywords", which are symbols in the [KEYWORD package](https://www.lispworks.com/documentation/HyperSpec/Body/11_abc.htm). A common use of keyword symbols is to avoid flooding a package namespace with symbols that will never be used again. If we wrote this defpackage form without keyword symbols, then symbols called `cl` and `play` would be pointlessy interned into the CL-USER package (or whichever package is active). In fact, as it stands, we're pointlessly interning the `beatles` symbol. We could avoid this by writing `(defpackage :beatles ...)` instead. The same applies for calls to `find-package`, as `(find-package :beatles)` works just as well as `(find-package 'beatles)` or `(find-package "BEATLES")`.

Anyway, let's now define some functions in the BEATLES package. Since the `play` symbol is exported, the function it identifies will be available outside the package.

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

This diagram represents how I think about packages. I will be gravely insulted if you don't find it helpful. The stickperson represents the active package.

<img src="{{ url_for('static', filename='img/cl-packages/example.png') }}"
     alt="3 boxes labelled 'common-lisp-user', 'beatles' and 'stones'. The first contains a stick figure and a few symbols: 'car, 'cdr, etc. The 'beatles' box contains 'play, 'sing, 'car (from CL), etc. The 'stones box contains 'play (from 'beatles'), 'sing, 'car (from CL), etc."
     class="centered">

We start off in CL-USER. The BEATLES package exports the `play` symbol, so we can call the function it identifies, but we don't have access to the `sing` symbol.

	:::lisp
	CL-USER> (beatles:play)
	badum tss
	NIL

	CL-USER> (beatles:sing)
	; (Error: The symbol SING is not external in the BEATLES package.)

If we REALLY want to access an internal symbol from a package, we can use double colon syntax. However, it's usually a bad idea to circumvent the interface of a package like that.

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

BEATLES and STONES share a single symbol by the name of "PLAY", since it was interned into BEATLES and then imported by STONES. In contrast, "SING" was interned separately into both packages and is mapped to two distinct symbols.

	:::lisp
	STONES> (eq (find-symbol "PLAY")
                (find-symbol "PLAY" (find-package :beatles)))
	T

	STONES> (eq (find-symbol "SING")
				(find-symbol "SING" (find-package :beatles)))
	NIL

## Package designators
We've seen that macros/functions like `defpackage` and `find-package` accept various data types as references to package, including symbols, keywords and strings. These are collectively referred to as [package designators](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#package_designator), defined in the standard as:

> a designator for a package; that is, an object that denotes a package and that is one of: a string designator (denoting the package that has the string that it designates as its name or as one of its nicknames), or a package (denoting itself).

A [string designator](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#string_designator), in turn, is defined as:

> a designator for a string; that is, an object that denotes a string and that is one of: a character (denoting a singleton string that has the character as its only element), a symbol (denoting the string that is its name), or a string (denoting itself). The intent is that this term be consistent with the behavior of string; implementations that extend string must extend the meaning of this term in a compatible way.

Here's a potential gotcha. You might think that `(defpackage 'apple [...])` is a valid call to `defpackage`, because `'apple` is a symbol, which is a package designator! But actually, `'apple` is short for `(quote apple)`, which is a list and decidedly NOT a package designator. When we *evaluate* `(quote apple)`, it returns the symbol called "APPLE", which would be a valid package designator, but when we pass `(quote apple)` to a macro then the macro just sees a list.

## What's the deal with the KEYWORD package?
We've already touched briefly on the KEYWORD package. In brief: keywords are symbols in the KEYWORD package, and `:blah` is syntax sugar for `keyword:blah`. But besides avoiding namespace pollution, what are keywords good for?

[Quoting the spec](https://www.lispworks.com/documentation/HyperSpec/Body/11_abc.htm):

> The KEYWORD package contains symbols, called keywords, that are typically used as special markers in programs and their associated data expressions. 

There are a few examples of keywords being used as "special markers", one of which is the use of keyword parameters in functions.

	:::lisp
	;; Defining a function with a keyword parameter.
	(defun foo (&key (x 0))
      (+ x 1))

	;; Here be a wild keyword!
	(foo :x 4) ; ---> 5

Another insightful quote from the spec...

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

...it behaves as we'd expect. If we'd instead implemented `favourite` using the comparison `(eq beatle 'john)`, then `(beatles:favourite 'john)` and `(beatles:favourite 'ringo)` would, perhaps surprisingly, *both* print "wrong", because the `john` symbol we'd be interning into the STONES package would be different to the `john` symbol in the BEATLES package.

In an act of unashamed plagiarism, here's a lazy quote of more usage advice from the spec that I thought was interesting:

>  It is generally best to confine the use of keywords to situations in which there are a finitely enumerable set of names to be selected between. For example, if there were two states of a light switch, they might be called :on and :off. 
>
>  In situations where the set of names is not finitely enumerable (i.e., where name conflicts might arise) it is frequently best to use symbols in some package other than KEYWORD so that conflicts will be naturally avoided. For example, it is generally not wise for a program to use a keyword as a property indicator, since if there were ever another program that did the same thing, each would clobber the other's data. 

## Can a symbol be homeless?
We've seen how symbols have a home package, which is the package into which they were originally interned. `(symbol-package 'sym)` yields this package. Here are some ways for a symbol to be orphaned, such that `(symbol-package 'sym)` returns `NIL`.

1) Manually create a symbol data structure: `(make-symbol "NAME-HERE")`.

	:::lisp
	CL-USER> (symbol-package (make-symbol "jinkies"))
	NIL

2) Syntax sugar for homeless symbols: `'#:name-here`. Note that `(eq '#:hi '#:hi)` returns `NIL`, since they're different symbols that just happen to share the same name. It's only within packages that each name is mapped to a unique symbol. If you want to be miserly about the number of junk symbols you're creating, then you can use these orphan symbols to denote package and symbol names in `defpackage`, `find-package`, `in-package` and elsewhere. I find it a bit verbose, though...

	:::lisp
	(defpackage #:beatles
	  (:use #:cl)
	  (:export #:play))

	(find-package '#:beatles)

3) `unintern` a symbol to remove it from a package:


    :::lisp
    CL-USER> (find-symbol "S")
	S         ; it exists!
	:INTERNAL ; and is not exported

	CL-USER> (unintern 's)
	T

	CL-USER> (find-symbol "S")
	NIL       ; it does not exist anymore!
	NIL

## Symbols and macros
Here's an example that should demonstrate the relationship between symbols, the Lisp Reader, and macros. Perhaps you're like me and came up with the ingenious idea of a macro that executes its body inside a certain package, before restoring the original active package. Something like...

	:::lisp
	(with-package :beatles
      (play))

And here's how you might imagine implementing this.

	:::lisp
	(defmacro with-package (name &body body)
	  (let ((old-pkg (gensym)))
        `(let ((,old-pkg *package*))
           (setf *package* (find-package ,name))
           ,@body
           (setf *package* ,old-pkg))))

Now, the above code should expand to something like...

	:::lisp
    (let ((#:g690 *package*))
      (setf *package* (find-package :beatles))
  	  (play)
      (setf *package* #:g690))

...where `#:g690` is a unique, homeless symbol produced for us by `gensym`.

The problem with this idea is that it represents a misunderstanding of how code execution works in Common Lisp. For the sake of simplification, let's say that code execution consists of two stages: Reading and Evaluation. First, raw text is guzzled up by the Lisp Reader and converted to Lisp data structures. It's in this stage that symbols are created, and it's here that their home package is determined, based on the value of `*package*` when Reading occurs. Next comes Evaluation, which begins with the expansion of all macros in the code. Macros are functions that operate on Lisp data structures and return Lisp data structures. By the time our `with-package` macro is passed the list `((play))` as its `body` parameter, it's already too late. The `play` symbol has been interned in whatever package was active when the Lisp Reader did its job, probably CL-USER. It doesn't matter if, when our expanded code is being evaluated, we set `*package*` before calling `(play)`, because the `play` has already been interned somewhere else.

All that to say: the Evaluation of a form can't affect how it is read by the Lisp Reader, since Reading happens before Evaluation. We could implement something like `with-package` using *reader macros*, but that's a topic for another day.

## Special symbols: T and NIL
There are two special symbols that evaluate to themselves: `t` and `nil`. When we evaluate `(symbol-value t)`, we get back... `t` again. These "special symbols" represent various things, including the boolean values true & false. `nil` also represents the empty list, hence why `(listp nil)` returns `t`.

	:::lisp
	CL-USER> (symbolp t)
	T   ; it's a symbol!

	CL-USER> (symbol-name t)
	"T" ; I'm not joking, it really is a symbol

	CL-USER> t
	T   ; evaluates to itself!

	CL-USER> (symbolp nil)
	T

	CL-USER> nil
	NIL

## When I compile a function, how does SLIME know which package it's in?
If you're an Emacs user and you've compiled a single Common Lisp form, like a function, using the `C-c C-c` shortcut, then you may have wondered how SLIME knows which package should be active when the Lisp Reader slurps in that form. For example, you might have the following code in a buffer:

	:::lisp
	(in-package foo)
	(defun bar ()
	  (+ 1 1))

In your REPL, the currently active package is CL-USER. So when you compile the `bar` function, why does it get added to the FOO package rather than CL-USER?

This is an implementation detail of SLIME more than anything, but I remember being confused by it, so let's indulge ourselves with this brief tangent. First, let's see which Elisp function the `C-c C-c` shortcut (Ctrl-C Ctrl-C) is bound to. Entering the help command for keyboard shortcuts, `C-h k`, and then `C-c C-c`, we're told that this shortcut is bound to the function `slime-compile-defun`.

This function is defined in `slime.el`. Following the link there and searching for "in-package", we find the following comment:

> We have the concept of the "current Lisp package". RPC requests always say what package the user is making them from and the Lisp side binds that package to \*BUFFER-PACKAGE\* to use as it sees fit. The current package is defined as the buffer-local value of 'slime-buffer-package' if set, and otherwise the package named by the nearest IN-PACKAGE as found by text search (cl-first backwards, then forwards).

And there's our answer: unless `slime-buffer-package` is set, SLIME first searches backwards and then forwards for the nearest IN-PACKAGE form. Not mentioned is that if no IN-PACKAGE form is found, then the code is compiled in the currently active package, whether that's CL-USER or something else.

For curiosity's sake, here's the Elisp function used to search for IN-PACKAGE.

	:::lisp
	(defun slime-search-buffer-package ()
	  (let ((case-fold-search t)
			(regexp (concat "^[ \t]*(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
							"\\([^)]+\\)[ \t]*)")))
		(save-excursion
		  (when (or (re-search-backward regexp nil t)
					(re-search-forward regexp nil t))
			(match-string-no-properties 2)))))

<hr />

<small>With thanks to ScottBurson, stassats, vindarel, zacque0 and kagevf for their helpful feedback on this post.</small>
