title: Symbols and Packages in Common Lisp
date: 2025-05-14
description: A journeyman's guide to symbols and packages.
requires: code
tags: lisp programming

Lisp programming languages have an extremely simple syntax, where everything is either a list or an atom. The most common type of atom is the **symbol**, which is used to name things like variables and functions. In the Common Lisp code below, `defun`, `pie`, `let`, `+`, `x` and `y` are all symbols.

	:::lisp
	(defun pie (x)
      (let ((y 1))
        (+ x y)))

When I started learning Common Lisp, I remember being confused by this superficially simple data structure, as well as the related concept of packages. This article is an attempt to explain symbols and packages to that past self.

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
If we enter just `x` at the REPL, it will be evaluated and the variable value associated with the symbol `x` (if such a value exists) will be the result. To avoid evaluating the symbol, we need to `quote` it, like `(quote x)`, or `'x` for short.

With this knowledge, let's test the statement above, that a symbol is NOT the same as its name. We need the `symbol-name` function to fetch the symbol name from the symbol data structure.

	:::lisp
    >>> (type-of 'x)
	SYMBOL
	>>> (symbol-name 'x)
    "X"
	>>> (equalp 'x (symbol-name 'x))
	NIL     ; not equal!

At read time, i.e. when the raw Lisp code is slurped in and parsed, each occurrence of a name will be mapped to the same symbol. And so, in this example, we're comparing the same symbol to itself.

	:::lisp
	>>> (equalp 'x 'x)
    T    ; equal

Common Lisp follows the [Lisp-2 model](https://stackoverflow.com/questions/4578574/what-is-the-difference-between-lisp-1-and-lisp-2), so functions and variables have separate namespaces. As such, a symbol can simultaneously identify both a function, which we fetch with `symbol-function`, and a variable, which we fetch with `symbol-value`. This is demonstrated in the following example, where the symbol `porridge` refers to both a function and a variable.

	:::lisp
    ;; Define function.
	>>> (defun porridge ()
          (+ 1 1))

    ;; Define variable.
	>>> (defparameter porridge 10)

	;; Symbol now refers to both a function and a variable.
	;; Get the value, get the function, and then evaluate the
	;; symbol as a variable and use it in a function call.
    >>> (list (symbol-value 'porridge)
              (symbol-function 'porridge)
              porridge
              (porridge)))
    (10 #<FUNCTION PORRIDGE> 10 2) ; <--- result

Each symbol also has an associated property list, accessed with `symbol-plist`. We won't get into that today. There's also `symbol-package`, which we'll discuss later.

That's most of the interface provided by the symbol data structure!

## Why is the symbol name in all-caps?
Good catch: we entered the symbol `'x`, but the printed symbol name was `"X"`. By default, all symbol names are converted to upper case by the so-called Lisp Reader. This can be controlled with a parameter called [readtable-case](https://www.lispworks.com/documentation/HyperSpec/Body/23_ab.htm), which, if you really want to preserve your precious little capitalisation, can be set to `:preserve`.

Honestly, I think this is one of the language's biggest warts. Under the default behaviour, if you're trying to implement a formula that contains - *gasp* - both `n` AND `N`, you're reduced to mutating one of the names like someone trying to meet password requirements by going from "password" to "Password2!".

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

We can reimplement `defun` as a 2-line macro:

	:::lisp
	(defmacro mydefun (name parameters &body body)
	  `(setf (symbol-function ',name) (lambda ,parameters ,@body)))

This somewhat cool fact was first demonstrated to me in Paul Graham's book, *On Lisp*.

(Disclaimer: I don't endorse Paul Graham's exaggerated proselytizing about Lisp, but it's a good book).

## Stupid symbol-value doesn't work!!
Based on what we've covered already, it'd be reasonable to expect the following code to work.

	:::lisp
	(let ((x 1))
      (symbol-value 'x))

But instead this raises an error about `x` being unbound.

Here's what's happening: `let` creates a [*lexical binding*](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lexical_binding) for the symbol `x`. Within the body of that `let`, `x` is lexically bound to the value `1`, but that doesn't affect the *global* value of `x`, which is what `symbol-value` tries to retrieve.

During evaluation, lexical bindings take precedence over global ones, so just evaluate the symbol to get its lexically bound value.

	:::lisp
	>>> (defparameter x 1)
	>>> (let ((x 5))
		  x)
    5

"Wow," you might be thinking, "Doesn't this make it really easy to accidentally shadow a global variable with a local variable?". Well, yes. That's why, similar to many other programming languages, Common Lisp has a naming convention for global variables. They given "earmuffs", so `x` becomes `*x*`.

## What is a package?
Symbols, as you can see, are quite simple data structures. Where it gets a bit trickier is in their interaction with **packages**. Again [quoting](https://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm) the spec:

> A package is a namespace that maps symbol names to symbols [...]

* Packages are Common Lisp's mechanism for creating namespaces. Your implementation of `foobar` can live alongside my implementation of `foobar` if they're stored 
* EXAMPLE
  package A exports 1 symbol, keeps 1 private
  show how to list all the symbols, inspect package to see what else there is to do
  package B uses package A, show how symbol-package references package A
  double colon to reference internal symbols
* defpackage
* in-package (to set *package*)
* (don't bother trying to manually set *package* in a macro, since it takes effect at readtime... this sidenote can be another section)

## Wait, I didn't explicitly add those symbols to the package, what's goin' on?
* Lisp Reader `intern`s all the names it encounters.
* Show example of manual `intern`.
* (intern "HiII")

## When I compile a single form, how does SLIME know which package it's in?
Show that SLIME looks for nearest (in-package ...) declaration.

If it can't find that, then it uses the current package?

## CL-USER, a.k.a. COMMON-LISP-USER
TODO

## Keyword package
keyword package `:hello` (awkwardness of using regular symbols as parameters in API)

## Orphan symbols
(...with no home package. how to create homeless symbol; how to unintern; i think there's special syntax for homeless symbols).

## Symbols and macros
* symbols get read by Lisp Reader in one package, they're now a data structure in memory, macro doesn't care where the symbols come from
* gensyms.
