title: Recurse Center, week 1
date: 2024-01-05
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc

### Day 1 (Wednesday, January 3rd)
Started with some presentations about the philosophy of the Recurse Center and how to get the most out of the experience. There's lots to learn about how RC works, from Zulip (their messaging system) to Virtual RC (a "virtual space" where you can move around your avatar and interact with other RCers). We also did some Chat Roulette-style meet-and-greets. Met maybe 15 people from my batch, but there are many others! Keeping a list of people with shared interests and who I think would be interesting to pair with. 

Between Zoom calls, I watched the first video in [Andrew Ng's machine learning course](https://www.youtube.com/watch?v=4b4MUYve_U8&list=PLoROMvodv4rMiGQp3WXShtMGgzqpfVfbU&index=2) on 2x speed. I'm not sure yet if I want to work through that course while at RC, but it seemed like a fun way to spend an hour. Considering whether to do the homework exercises in J, but not very confident in my J skillz.

Feeling tempted by people talking about their programming language projects, makes me want to dive into the *Crafting Interpreters* book, or maybe *Engineering a Compiler*.

### Day 2 (Thursday, January 4th)
Started the day with a workshop on pair programming, which they're strongly encouraging us to do. I paired up with Reed to work on a Mastermind clone in JavaScript. He shared some of his JavaScript philosophy, and made some analogies between the DOM and the sorta environment you'd have in Smalltalk/Erlang... both of which are languages I wanna try out. Consider function calls as "messages" to elements in the DOM. We made a pretty-looking board with some interactivity, but didn't implement the game logic. Reed is good at this JavaScript thing! He also showed me a cool Emacs package for interactively writing regex, I will definitely add it to my Emacs config.

<figure>
<img src="{{ url_for('static', filename='img/recurse/mastermind.png') }}"
     alt="Our basic interface for the Mastermind boardgame, with round coloured circles for pegs and a submit button."
     class="centered">
<figcaption>Our Mastermind clone.</figcaption>
</figure>

I'm going to fill in this blog post as I go along, seems easier than trying to remember everything at the end of the week.

Next up, I wanted to get my feet wet and actually start a programming project. What better for that than a small generative art piece! I got the gist of how to visualise the Lorenz attractor from a [Coding Train](https://thecodingtrain.com/challenges/12-lorenz-attractor) video, which I then went ahead and did.

The only stumbling block was figuring out whether I could draw it in 2d, given that [sketch](https://github.com/vydd/sketch), the Common Lisp drawing framework I use, does not support 3d and that The Coding Train's version is in 3d. I realised I could just ignore the z dimension when drawing, and I could use the z value to determine the colour of the line.

<figure>
<video width="400" height="400" class="centered" controls>
    <source src="{{ url_for('static', filename='video/recurse/lorenz.mp4') }}" type="video/mp4">
</video>
<figcaption>The Lorenz attractor!</figcaption>
</figure>

I made it so that pressing the space key resets the drawing and randomises the parameters. It was kinda awkward to record this video, though -- I had to rush to press the 'record' button after starting the animation. So tomorrow I think I'll work on adding to sketch the ability to save a video of an animation, probably by piping video output to ffmpeg.

I ended the day by starting to read about the [BitTorrent protocol](http://www.bittorrent.org/beps/bep_0003.html), since I want to implement it in order to learn more about computer networking.

### Day 3 (Friday, January 5th)
Tried to catch up with all the messages on Zulip -- there are a lot! At some point I'll have to filter everything down to make it more manageable. For now, I'm rolling with the chaos.

Started my BitTorrent client project. Found that [someone else](https://github.com/andrei-dubovik/centrality) has already implemented such a client in Common Lisp. I don't intend to use their code, but it will be useful to have a reference and to see what packages they used.

After skimming the [specification](http://www.bittorrent.org/beps/bep_0003.html), I jumped into writing a parser for the bencoding text format. It has integers like "i123e" and strings like "4:spam", as well as lists and dictionaries. I've been meaning to try out [esrap](https://github.com/scymtym/esrap) for parsing in Common Lisp, and this seemed like the perfect opportunity! It uses the idea of parser generators, which I think are like composable building blocks for parsing.

I ran into some early trouble with esrap due to an annoying technical issue. Basically, I was trying to use the '?' symbol from the esrap package without having access to it. This was frustrating because it felt like I was squandering my day on a trivial issue.

There are edge cases in even the simplest of tasks, like parsing bencoded integers: "i0e" is valid, "i01e" is invalid, and "i-0e" is invalid. On that note, esrap has nice error messages!

	:::lisp
	BENCODE> (parse 'benteger "i01e")

	[...]

	At

	  i01e
		^ (Line 1, Column 2, Position 2)

	In context INTEGER:

	While parsing INTEGER. Expected:

		 the character e (LATIN_SMALL_LETTER_E)
	   [Condition of type ESRAP-PARSE-ERROR]

Here's a taste of what the DSL looks like for defining grammar rules! Courtesy of macros. Maybe I should give a presentation on macros, even though I'm still learning about them.

	:::lisp
	(defrule benteger
		(and #\i
			 (or "0"
				 (and (? #\-)
					  nonzero
					  (* digit)))
			 #\e))

Parsing bencoded strings is a little tricky, because the length of the string is determined by the integer prefix. I had to go outside the constraints of the `defrule` syntax and write a custom `parse-bencoded-string` function, which, thankfully, integrates well with esrap parsing. I was then able to define the rest of the parser in 4 lines of code:

	:::lisp
	(defrule str (function parse-bencoded-string))
	(defrule ben (or str benteger list dict))
	(defrule list (and "l" (* ben) "e") (:function second))
	(defrule dict (and "d" (* (and str ben)) "e") (:function second))

Testing:

	:::lisp
	BENCODE> (parse 'list "l4:spam4:eggse")
	("spam" "eggs")
	BENCODE> (parse 'dict "d3:cow3:moo4:spam4:eggse")
	(("cow" "moo") ("spam" "eggs"))


Pretty neat! [Here's the code so far](https://github.com/Kevinpgalligan/cl-torrent). Next week I'll come back to this and write unit tests to make sure I've covered all the edge cases. I haven't done unit testing in Common Lisp before, so it'll require some effort to set up. Seems like a good candidate for pair programming!

I'm trying to reach out to people with similar interests, but it's hard to keep track of everyone. I'll start attending the daily check-ins next week, and also some of the meetings like "Creative Coding" and Emulators + VMs", which will hopefully lead to new connections.

To finish the week, I made a 2-line [pull request](https://github.com/vydd/sketch/pull/133) to [sketch](https://github.com/vydd/sketch) (don't draw a border around images by default!) and described my proposal to the other developers about how to fix a tricky bug where the first render call gets lost due to the window getting resized.

Posting this now because I want my weeks to start on a Monday and end on a Friday. Hopefully I'm not being too verbose!
