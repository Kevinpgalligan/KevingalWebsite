title: Recurse Center, week 4
date: 2024-01-26
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc

### Monday, January 22nd
Did more work on my pull request for ~sketch~, feels like a disproportionate amount of effort to fix this bug.

Read some [jonesforth](https://github.com/nornagon/jonesforth/blob/master/jonesforth.S) and [Starting Forth](https://www.forth.com/starting-forth/11-forth-compiler-defining-words/), trying to understand Forth execution.

My original plan was to write most of my Forth code in a Lisp DSL, like this. 

    :::lisp
    (defword 2dup (dup dup))

Forth code is then just a list of Lisp symbols!

However, this has various hairy edge cases. Consider the Forth words `(` or `:`, neither of which are valid names for Lisp symbols. A possible solution is to expand these into Latin characters with some type of escape character in front of them: `^lparen` and `^colon`, for example. The escape character itself would have to be escaped, too, in case the user ever tries to define a word called `^lparen` -- with escaping, this would become the Lisp symbol `^^lparen`. And the Forth word `^(` would become the symbol `^^^lparen`.

Another edge case to avoid: imagine I gave the symbol `|` the expanded name `^lpar`. Then the Forth word `|en` would expand to `^lparen`, clashing with the Lisp symbol for `(`. So the expansions of banned symbols cannot be prefixes of each other. This probably comes up a lot when designing programming languages (string escaping!), so that could be interesting to look into.

I think that solves the problem of weird Forth characters. However, there's also the problem that Forth words are able to hook into the parsing of the program they're part of. Famously, comments in Forth are implemented as the words `(` and `)`. Here's how they're used:

    :::forth
    : someword ( this is a Forth comment ) 1 1 + ;

The definition of `(` is basically: skip text until the next right parenthesis shows up. That wouldn't work with my "list of symbols" DSL because the symbols will be parsed by the Lisp reader and there will be no input stream of text to speak of.

Another Problem Word is `".`, which prints a string to the screen...

    :::forth
    : firstprogram ". Hello World " ;

...which might be defined in the DSL (with escaping for the `"` special character) as follows...

    :::lisp
    (defword firstprogram (^quote. Hello World ^quote))

But, again, `".` needs access to the input stream of text. So, I think the DSL should allow Forth words to be defined as strings...

    :::lisp
    (defword firstprogram "\". Hello World \"")

Anyway, this is all a distraction from the task of actually implementing a Forth. What's confusing me more is how to map the Forth [memory model](https://www.forth.com/starting-forth/9-forth-execution/#Forth_Geography) onto a running Lisp process. Forth seems to assume that it's operating on a giant contiguous block of memory, and it compiles itself by writing stuff into that memory. That's fine for assembly language, but I really don't get how to deal with that in Common Lisp or JavaScript or whatever. The Forth word `HERE`, for example, points to the address space where the next dictionary entry should be written. What does that mean when the dictionary is a list data structure in JavaScript or Common Lisp or whatever? I think it would be straightforward in C: the dictionary is a big `malloc`'d block of memory, and we can push onto the stack the actual memory address where the next DictionaryEntry struct should be written.

After some thought (and reading stuff, and pestering Reed), I think I should treat Forth memory as a giant array and write the dictionary (as well as text input and everything else) directly into it as bytes!

My brain was mush from all the reading and thinking, so I finished the day by doing "mindless" coding: turning my `vec2` implementation into `vec`, for whenever I need to do physics or graphics stuff in more than 2 dimensions.

### Tuesday, January 23rd
I spent a lot of time working on ~sketch~ today (now using "~" to demarcate when I'm talking about the Common Lisp graphics library called "sketch"). First, refactoring my pull request according to Gleefre's (one of my co-developers) feedback. Then testing a fix that Gleefre finished for drawing polygons. Then brainstorming various possible features and documenting them as issues in the repository. My testing did result in a kinda cool animation (**EPILEPSY ALERT!**):

<figure>
<video width="100" height="100" class="centered" controls>
    <source src="{{ url_for('static', filename='video/recurse/polygons.mp4') }}" type="video/mp4">
</video>
<figcaption>Drawing a random polygon with 10 vertices, many many times per second.</figcaption>
</figure>

Since then I've been catching up on my blogging and thinking a lot about how to implement a Forth interpreter (most of the brainstorming got dumped into DMs with Reed and into Monday's entry).

I haven't been participating so much in social activities at RC! Partly because my projects don't currently overlap with most of the groups, and partly because I'm feeling a little low on social energy to be pairing / chatting with people. Trying not to be too hard on myself over this.

### Wednesday, January 24th
Had a nice call with David about programming and BitTorrent stuff.

The rest of the day was consumed by Creative Coding, where the prompt was "Impossible objects (undecided geometry)". I took the opportunity to draw some trees based on Coding Train videos: recursion trees, object trees (with leaves! 🍃), and L-system trees.

<figure>
<img src="{{ url_for('static', filename='img/recurse/recursive-tree.png') }}"
     alt="A recursive tree."
     class="centered">
<img src="{{ url_for('static', filename='img/recurse/leaves-again.png') }}"
     alt="An object-oriented tree."
     class="centered">
<img src="{{ url_for('static', filename='img/recurse/lsystem-tree.png') }}"
     alt="An L-system tree."
     class="centered">
<figcaption>Trees!</figcaption>
</figure>

The code is [here](https://github.com/Kevinpgalligan/sketches/blob/master/src/thesketches/trees.lisp). There's a memory fault after more than 6 iterations of the L-system, with no backtrace. I think it allocates too much memory because the strings grow surprisingly quickly -- if my napkin calculations are correct, ~1GB after 7 iterations. The solution may be to do depth-first expansion of the rules, rather than breadth-first, so that no string allocation is necessary. (Although, it sounds like it would take forever to iterate through 1 billion symbols, so it may be impractical to go beyond a depth of 6 for this L-system, anyway).

When I get that working, I may as well test out all the L-systems on the [Wiki page](https://en.wikipedia.org/wiki/L-system), since the rules can be copy/pasted once you have an L-system implementation.

    :::lisp
    (def-lsystem tree
        ((#\F "FF+[+F-F-F]-[-F+F+F]")))
    (evaluate-lsystem 'tree :axiom "F")

### Thursday, January 25th
Read through a paper on the [Paxos algorithm](https://en.wikipedia.org/wiki/Paxos_(computer_science))  with Chris, courtesy of the Pairing Bot. The paper was surprisingly entertaining! It's an algorithm for reaching consensus among distributed processes, but the author framed it as "priests" voting in a "legislature" on the island of Paxos.

Spent some time on my generative trees (see above).

Returned to the BitTorrent client! It feels less intimidating now that I've broken the project into smaller pieces. After skimming [this article](https://blog.jse.li/posts/torrent/) shared with me by David, I've kinda changed my mind about how to handle multi-threading in the client. I'm leaning towards doing all thread-to-thread communication using queues, because it seems easier to reason about than sharing a big data structure with a lock on it.

Today's main coding task was to write a buffer / parser for peer-to-peer messages (network bytes in, messages out). Almost at the point of writing unit tests for it. Other small tasks I've identified:

* A data structure to accumulate the blocks that form pieces, before they're stitched together.
* Validating the SHA1 hash of completed pieces.
* Writing pieces to the appropriate file on disk.
* Implement the bitmask for tracking which pieces have been downloaded.
* Make a flowchart for the lifecycle of the threads that talk to peers.

I ended the day by attending the weekly presentations. They were all amazing, but particular highlights were: Tristan's singing computer (using a one-tone beeper to play music by beeping it at different frequencies), Jacob's custom MIDI device, and Julie's AI fishtank TV show (object and collision detection + a fish tank + games for the fish to unknowingly play = an automated version of an Adult Swim comedy show). I find myself getting very excited about music-related projects, so I should probably launch into music programming when my current projects are wrapped up.

### Friday, January 26th
Inspired by Isaac, I went for a walk before doing any programming today. Also kicked off MemTest86 tests on my desktop, since it has suffered from intermittent freezes ever since I built it. If that doesn't turn up anything, I guess I'll open it up and make sure all the components are properly secured. Couldn't hurt to update Linux Mint, either.

I finished my goal of implementing a message buffer interface for the BitTorrent client! It accumulates bytes from network packets, parses any peer-to-peer messages in that stream of bytes, and spits those messages back out! Also wrote a bunch of unit tests, which reminded me of my old job -- we had to write a lot of unit tests! In the process, I discovered that Common Lisp has built-in support for bitmasks, so I won't need to implement them myself.

    :::lisp
    ;; A bit vector of 7 bits!
    (defparameter *bs* (make-array 7 :element-type 'bit))
    (bit-and *bs* #*1111000 t) ; make the last 3 bits 0!

I did remember another task I'll have to take on: serialising peer-to-peer messages for network transmission. I'm sure there's a library I could've used for defining the serialisation format rather than manually writing code to do the serialising AND deserialising. Oh well, it was nice to make some progress today.

Besides that, I did a Forth deep dive with Reed, trying to figure out the execution model of Forth and how to model it in a higher-level language. He also shared some neat Emacs tips with me ("use Doom").

Plan for next week:

- More gradual progress on BitTorrent.
- Get to a point where I can actually start writing code for the Forth interpreter.
- Some generative art / ~sketch~ stuff.
