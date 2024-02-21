title: Recurse Center, week 7
date: 2024-02-16
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc
tagcount-exclude: y
rss-exclude: y

#### Reflections
Responding to some prompts from the half-way reflections event!

- *What’s something you’re really proud of from your time in batch so far?* I'm happy with the contributions I've made to [Sketch](https://github.com/vydd/sketch), and I'll be proud of my BitTorrent client & Forth interpreter when they're done -- they will be among the most complex pieces of software I've written in Common Lisp. I'm also proud of how consistent I've been with my checkins!
- *What do you wish you’d done differently?* I wish I'd scoped the BitTorrent project better. It turned out to be bigger than expected, and I feel like I've spent a lot of time writing Business Logic rather than learning new networking concepts. It also didn't lead to much interaction with other Recursers. It might've been better to start with something small like [implementing DNS](https://implement-dns.wizardzines.com/), which would've had a higher density of learning and would've been easier to pivot from. I'm in an "I just want to get it done" mode, which isn't how I want to feel about my projects at RC. I want to feel "wow, this is so fun, can't wait to work on this today!". Lots to learn about how to choose my projects going forward!
- *How have you grown at RC, and how do you want to keep growing as a programmer?* I've definitely learned new things! How to implement network protocols; parser combinators; I've become more fluent in Common Lisp; generative art stuff; Magit for doing git in Emacs; how Forth works; and I've been exposed to a lot of ideas by following what other people are working on. The experience has reignited my passion to become a better programmer -- I think I stagnated for a long time because I was demotivated by my PhD. Now I'm back on track and I'm ticking things off from my programming/learning bucket list!

The plan going forward:

- Finish my current projects (BitTorrent and Forth).
- Start some smaller projects that involve more Concentrated Learning and Sociability. Candidates so far: ELF parser, CTF challenges, graphics / shaders, music programming with SuperCollider.
- Learn more generously! This means picking my projects more carefully, attending groups, reaching out to the many people I want to talk to/pair with, and presenting my work.
- Update my CV and start engaging with the RC careers people!

### Monday, February 12th
Got distracted today reading [Learn You a Haskell](https://learnyouahaskell.github.io/chapters.html), which I last visited 5+ years ago. Really enjoying all the concepts it's exposing me to! I've realised that I may be leaning too heavily on the imperative programming features of Common Lisp (hard to resist the power of the `loop` macro), and this seems like the perfect antidote.

### Tuesday, February 13th
I was traveling today and didn't end up doing any programming. I mostly spent the day reading [Learn You a Haskell](https://learnyouahaskell.github.io/chapters.html) on my phone. I feel like I'm getting the concepts, but if I tried to actually write Haskell code I would probably drown in compile errors. Here's a braindump of what I've read so far, which is intended less for human consumption than as a way to revise the concepts for myself.

(*Warning!* Scary-sounding words like "functor" and "monoid" incoming, which I did not understand 1 week ago and only have a vague understanding of now, but which aren't so bad once you start reading about them, and actually seem quite cool/useful!).

I'm up to [Chapter 11: Functors, Applicative Functors and Monoids](https://learnyouahaskell.github.io/functors-applicative-functors-and-monoids.html). I was able to skip and skim a lot of the earlier chapters because I still remember Haskell stuff from when I tried learning it a few years ago.

Preliminaries: In Haskell, `f x` calls a function `f` with the argument `x`. It's like writing `f(x)` in other languages. Haskell has a complex type system. You can define your own types and define their behaviours using...

...typeclasses. If  a type is a member of a certain typeclass, then it implements the functions defined for that typeclass. EXAMPLE: `Orange` and `Apple` might be part of the `Fruit` typeclass, and might provide implementations for a function called `peel`. This is somewhat like interfaces in other languages, where typeclass = interface and types = classes that implement the interface.

Next: the `Functor` typeclass. It's a scary-ass word, but it's just a type that you may be able to get values out of, and that implements the `fmap` function for "mapping" a function onto those values. A list, for example, is a functor, 'cause you can get values out of it, and you can apply a function to all the values of a list using `fmap f list`. Functions themselves are also functors, because you can get values out of a function by calling it! `fmap` for functions is actually function composition, i.e. `(fmap f g) x` is equivalent to `f (g x)`. This makes sense from my handwavy explanation: fmap applies a function to the values outputted by a functor -- in this case, the functor is a function, and so fmap ends up chaining the two functions together. The concept of a functor is general enough that it applies to lots of other things: all I/O in Haskell is wrapped up in a functor type called `IO`, for example.

An example of how the `Functor` typeclass is useful: if you make a new `Tree` type, and make it part of the `Functor` typeclass, then you can map functions over the values in the tree. Also, we can now write code that can be reused by any `Functor` type!

There are rules that `Functor` types should follow, but that are not enforced by the language. I'm not sure why there are rules, and I'm not sure why the language doesn't enforce them. ANYWAY -- they are (I think): (1) fmap-ing the identity function onto the functor (`fmap id x`) should be the same as directly calling the identity function (`id x`); (2) fmap should be linear (I think that's the right word?), i.e. fmapping the composition of two functions should give the same result as fmapping each of them in turn: `fmap (f . g) x` = `fmap f (fmap g x)`.

Next: applicative functors. The book describes these as "beefed-up functors", i.e. functors with some extra useful properties. These types implement a `pure` function, which takes a value and returns an instance of the functor that always returns that value. The implementation of `pure` for lists just returns a list with the value you gave it: `pure x = [x]`. `pure` for functions (which are another type of applicative functor) returns a constant function: `pure x = \_ -> x` (that's a function that ignores its parameter and always returns `x`). Applicative functors also implement the `<*>` function. This is like `fmap`, except the function is provided by a functor. Back to the list example: `[3*] <*> [1,2]` returns `[3,6]` -- `3*` is a function that multiplies its argument by 3, so `<*>` essentially extracts that function from the list and applies it to each of the elements of the list, and the output is still a list. Note: functions like `<*>` that consist only of special characters can be written in infix notation, or they can be written in prefix notation if you surround them with brackets, so `x <*> y` is the same as `(<*>) x y`. It's ambiguous what should happen if the function list is the same length as the target list, like `[3*, 2+]`. Do we pair up each function with a value from the target list (yielding `[3,4]`), or do we apply each function to all the values of the target list (yielding `[3,6,3,4]`)? The Haskell designers settled on the latter, although they do provide another list type that behaves like the former. That's pretty cool! Using `f <$> x`, a shortcut for  `fmap f x`, we can generate the addition tables for the numbers 1-12 by doing `(+) <$> [1..12] <*> [1..12]` (I think -- haven't actually tested this).

The implementation of `<*>` for functions is where things get a bit complex and my understanding is wobbly. Since `<*>` expects as its first argument a functor that outputs a function, and here our functor is itself a function, the first argument of `<*>` must be a function that returns a function: the type signature of the function is `r -> a -> b` (takes an `r` and outputs a function `a ->b`; or, equivalently, takes  an `r` and an `a` as input and outputs a `b`). The next argument of `<*>`, like `fmap`, is a functor that outputs values of type `a`, i.e. in this case it's `r -> a`. The output of `<*>` should be a functor of type `r -> b`.

Recap: `<*>` expects 2 functions, whose type signatures are `r -> a -> b` (let's call it `f`) and `r -> a` (call it `g`). We want to combine them somehow to produce a function of type `r -> b`.

One way to do that, and the way the Haskell designers did it, is like this: `f <*> g = \x -> f x (g x)`. Note that if the argument `x` is of type `r`, then `(g x)` outputs something of type `a`. `f x (g x)` thus invokes the function `f` on arguments of type `r`and `a`, as required! And the output is of type `b`. So overall, `<*>` gives us a function that takes an `r` and outputs a `b` :yes:

Applicative functors are also expected to satisfy certain rules, which I won't list here. I think it's all rooted in ideas from category theory, which is probably where the rules come from. And I suspect that the implementation of `<*>` for functions comes naturally from satisfying the rules?

There's an interesting connection here with the J programming language! `f <*> g` has the same affect as a "hook" in J. Except, in J, it's implicit, and you can combine functions in these fancy ways just by having them next to each other: `(f g) x`. More generally, I think this is called a combinator in combinatory logic? My knowledge of these things is all very vague.

Last typeclass for today: monoids. I'm still learning about monoids, but they seem pretty similar to groups from group theory. You have a set of elements, a binary operator on those elements, and an identity element that, when combined with another element using the binary operator, just outputs the other element. Also, the operator must be associative: `x op y op z`= `(x op y) op z` = `x op (y op z)`. An example would be the non-negative integers (0, 1, 2, ...) and addition (+). Here, the identity element is 0, and `1+(2+3)` = `(1+2)+3`. From reviewing [Wikipedia](https://en.wikipedia.org/wiki/Algebraic_group), the only thing missing to make a monoid a group is that the elements don't have to have inverses (when you combine an element with its inverse, you get back the identity element, e.g. 1+(-1)=0 -- but for monoids you don't need 'em).

The set of all lists, paired with the concatenation operator (`++`),  is a monoid! The identity is the empty list, `[]`: `[1,2] ++ [] = [] ++ [1,2] = [1,2]`. And concatenation is associative: `([1,2] ++ [3]) ++ [4,5]` = `[1,2] ++ ([3] ++ [4,5])` = `[1,2,3,4,5]`.

#### Wednesday, February 14th
Finished reading Chapter 11 of Learn You a Haskell, I think I kinda know what a monoid is now? Anyway, time to put that distraction aside and get back to BitTorrent.

Attended Chris's Paxos talk while walking home in the rain and carrying a massive backpack + shopping bag, disrupted the talk when I joined because I was simultaneously trying to put on a raincoat and didn't realise my microphone was on. Woops, embarrassing. I don't know much about the theory of distributed systems, despite my old job centering around a distributed database, so it has been really cool to see this Paxos project unfold and learn a bit about it!

I wrote about my experience at the Center for Computing History in Cambridge. May or may not convert that from a Zulip message to a blog post.

#### Thursday, February 15th
I was f-ing right! The bug in my BitTorrent test was due to character encoding. It's ALWAYS character encoding.

Basically, the web server I was using as a dummy tracker was trying to parse all the parameters in the URL and convert them to UTF-8 strings. This URL:

    http://127.0.0.1:4242/announce?field=%27%10%C5

...causes an error because decoding the percent-encoded string "%27%10%C5" does not yield valid UTF-8. I've opened an [issue](https://github.com/edicl/hunchentoot/issues/226) in the repo of the web server. In the meantime, I've switched to a simple Python-based web server. This problem, and trying to work around it, ended up sapping a lot of time today.

### Friday, February 16th
The presentations were awesome yesterday! Literally all of them! From a Forth presentation that was itself a valid Forth program, to cool visualisations in Jupyter Notebook, to a homemade Digital Audio Workstation.

It's surprisingly difficult to write unit tests for the Brain of the BitTorrent client. The client class contains a LOT of state, so I tried using a mocking library to stub out said state. I couldn't get the mocks to work, and on reflection, it actually wouldn't be too hard to create real test data for the tests, so that's probably what I'm gonna do instead.

Next: after my testing failure, I wanted a quick win, so I tried writing a simple rasteriser to draw skyscrapers (a.k.a. boxes). However, the graphics library I'm using doesn't support 3D. I started looking at how to model a camera & do the projection-onto-2d and soon realised there would be no "quick" win. Everything is hard, ahhhh!
