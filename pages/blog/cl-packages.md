title: Symbols and Packages in Common Lisp
date: 2025-05-14
description: A guide to Common Lisp's concepts of symbols and packages, for Common Lisp beginners.
requires: code
tags: lisp programming

Lisp programming languages have an extremely simple syntax, where everything is either a list or an atom. The most common type of atom is the symbol, which is used to name things like variables and functions. In the Common Lisp code sample below, `defun`, `pie`, `let`, `+`, `x` and `y` are all symbols.

	:::lisp
	(defun pie (x)
      (let ((y 1))
        (+ x y)))

When I started learning Common Lisp, I remember being confused by this apparently simple data structure, the symbol, as well as the packages that symbols live in. This is the blog post I wish someone had written to explain them to me.

<figure>

<img src="{{ url_for('static', filename='img/cl-packages/cl-user.png') }}"
     alt="A box labelled 'common-lisp-user', with a stick figure inside and two symbols, 'car' and 'cdr'.."
     class="centered">
<figcaption>The common-lisp-user package and some of the symbols it contains.</figcaption>
</figure>

[toc]


## What is a symbol?
When it comes to definitions, it's helpful to consult Common Lisp's ANSI specification. [Quote](https://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm):

> Symbols are used [...] to name various entities in Common Lisp, including (but not limited to) linguistic entities such as variables and functions.

In other words, we can consider a symbol to be a data structure for identifying things. It's important to emphases that **symbols are not strings**. Each symbol has an associated name, which is stored as a string, but the symbol itself is not a string.

How do we reference symbols and manipulate them? If we enter just `x` at the REPL, the symbol will be evaluated and the variable value associated with `x` (if this value exists) will be fetched. To avoid evaluating the symbol, we need to `quote` it, like `(quote x)`, or `'x` for short.

Now let's test the statement above, that a symbol is NOT the same as its name (duh). We need the `symbol-name` function to fetch the symbol name, which is a string, from the symbol data structure.

	:::lisp
	>>> (symbol-name 'x)
    "X"
	>>> (equalp 'x (symbol-name 'x))
	NIL     ; (false)

Perhaps this is belabouring the point a little, but I want to make sure there's no confusion.

(Wondering why the X is upper-case? We'll get to that shortly).

Go through other things stored in symbol data structure:

* (symbol-function 'blah)
* (symbol-package 'blah)
* Maybe give example of how defun is basically syntax sugar over setf.

## Reading and Evaluating
Symbol gets slurped in by reader, intern'd (so that each identifier is mapped to a single symbol), then later on gets evaluated.
By default, they're upper-cased. I don't like this, even though it can be changed by default.

    (let (BlAhHhH)
      (symbol-name 'blahhhh))

## Packages
Basically a bucket of symbols. CL-USER, short for whatever. Which I think pulls in from various core packages...

Each symbol belongs to a home package.
    (symbol-package 'blah)

If a package USES another package, then it imports all the symbols into its namespace. Show how you can see which packages a package uses.

## Inline compilation in SLIME
Show that SLIME looks for nearest (in-package ...) declaration.
If it can't find that, then it uses the current package.

## Other
* (intern "HiII")
* internally, each symbol is compiled to a single identifying number, which makes them quick to compare.
* keyword package `:hello`
* awkwardness of using regular symbols as parameters in API.
* raw (?) symbols `'|Hello|`
* orphan symbols, no home package.
* macros.
* other package concepts.

# Graveyard
 
