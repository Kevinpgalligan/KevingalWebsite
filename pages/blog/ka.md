title: Making a calculator
date: 2022-03-30
description: A short description of a calculator I've been working on.
imgthumbnail: img/thumbnail.jpg
publish: y
tags: programming

I've released a [calculator language](https://github.com/Kevinpgalligan/ka), codenamed *ka*! It can do quick one-off calculations from the command line (such as `ka '9*9'`), or it can be run as a command line interpreter:

    $ ka
    ka version 1.0
    >>> 2 * (1/2)
    1
    >>> 1 metre + 1 foot > feet
    4.2808398950131235
    >>> p = 0.7; C(10,3) * p^3 * (1-p)^7
    0.009001692000000007
    >>> sin(90 deg)
    1 
    >>> e^pi
    23.140692632779263

It also has a crappy work-in-progress graphical interface, which can be summoned by running `ka --gui`. 

<figure>

<img src="{{ url_for('static', filename='img/ka/gui.png') }}"
     alt="ka GUI, which basically consists of a single line to enter expressions and a white text area where the results are printed."
     class="centered">

<figcaption>In which Kevin learns the frustrations of GUI programming.</figcaption>
</figure>

I decided to make a calculator because I found myself sometimes using Python as a calculator (for scipy's math functions), sometimes using the desktop calculator that comes with Linux Mint (for convenience), sometimes using the Common Lisp REPL (for its first-class support of fractions), sometimes using web calculators. I wanted all the functionality that I commonly use to be available in one, convenient package.

One thing that needs work, if I want anyone besides myself to actually be able to use ka, is the packaging. I haven't tested how easy it is to install from scratch, but a Linux user would need to (1) install the Qt 5 GUI framework using their package manager of choice, (2) use pip to install the ka-cli package from the Python Package Index, and (3) add an entry to their desktop configuration so that they can run ka as a standalone application rather than having to start up a terminal every time. Ideally, this would all be done by an installer, but I haven't packaged an application for Linux before and I'm too lazy to learn how. And let's not talk about Windows.

The most interesting things about ka on the language level are its

1. Turing-incompleteness (no loops or conditionals), which is fine for a calculator language.
2. Support for units.
3. Type system.

I will briefly discuss each of these points!

I've decided not to add loops or if statements or many features of Real Programming Languages because it seems like unnecessary bloat for a calculator. I did want to include support for variables, however, because otherwise certain common calculations would be a pain in the neck and I'd have to go crawling back to Python all the time. If a calculation truly requires loops or conditionals, well then maybe it should be done using a Real Programming Language after all.

It's kinda interesting how the unit system works. The language has a Quantity type, which consists of (1) a magnitude and (2) a unit. "5 metres" is a quantity whose magnitude is 5 and whose unit is, well, the metre. Quantities that measure the same physical characteristic (such as 'length') can be added and subtracted. It doesn't make sense to add quantities that measure different things, like 'area' and 'velocity'. Any quantities can be multipled or divided together, however. For example: if you know that a bath's volume is `V=3 metres^3`, and it's filling up at rate `r=0.01 metre^3 | second`, then `V/r` gives the time in seconds until the bath fills up.

Internally, all the units are derived from the 7 [SI base units](https://en.wikipedia.org/wiki/SI_base_unit). The newton, the unit of force, is written in terms of base units as `kilogram ⋅ metre ⋅ second^-2`. I'm not sure where I got the idea (probably while skimming the Wikipedia page on [dimensional analysis](https://en.wikipedia.org/wiki/Dimensional_analysis)), but I coded this up by storing the integer exponent of each base unit in a 7-dimensional vector. If the 1st dimension is devoted to kilograms, and the 2nd dimension to metres, and the 4th dimension to seconds, then the newton's vector representation would be `<1, 1, 0, -2, 0, 0, 0>`.

When you multiply two quantities, their vectors are added together to make a new quantity. When you divide them, the second one is subtracted from the first. So, returning to the bathtub example, the volume would have a vector representation of `<0, 3, 0, 0, 0, 0, 0>`, the rate would be `<0, 3, 0, -1, 0, 0, 0>`, and `V/r` would be `<0, 3-3, 0, 0-(-1), 0, 0, 0>`, or just `<0, 0, 0, 1, 0, 0, 0>` (seconds). Units can also come with a multiplier (the foot is a multiple of the metre) or an offset (the degree Celcius is offset from the kelvin). And they can have prefixes that apply a multiple: kilo multiplies by 10<sup>3</sup>, centi multiplies by 10<sup>-2</sup>. That's about it for units! Though of course, there are some edge cases, such as the kilogram being the only base unit with a prefix.

The type system is strongly typed, which means it complains when you pass a Fraction to a function that expects an Integer. There are basically two types of types: numeric types, which are arranged in the hierarchy Real < Fraction < Integer, and a Quantity type all on its own. An Integer can pass as a Real, because it's further down the hierarchy, but not vice versa. I got the type hierarchy idea from Structure and Interpretation of Computer Programs (SICP). Another trick I stole from SICP: if a Fraction can be simplified to an Integer, then this is done automatically, so that `6/2` is always simplified to `3`. The implementation language of ka is Python, and the fact that Python has a module for [fractions](https://docs.python.org/3/library/fractions.html) and a module for [numeric types](https://docs.python.org/3/library/numbers.html) made my work on the type system a lot easier.

And there you have it. A calculator language in about 1400 lines of Python code. If anyone happens to read this and happens to try out ka, I'd be curious to hear how you get on with it!
