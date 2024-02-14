title: Recurse Center, week 5
date: 2024-02-02
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc
tagcount-exclude: y

### Monday, January 29th
I was tweaking my code for the L-system tree, turns out the tree was MUCH bigger than the window I was drawing it in. Looks better when zoomed out!  The code now uses recursion & depth-first expansion instead of a loop & breadth-first expansion -- goodbye memory exhaustion!

<figure>
<img src="{{ url_for('static', filename='img/recurse/lsystem-d4.png') }}"
     alt="An L-system tree, highly recursive structure, pink background and wheat-ish colour."
     class="centered">
<figcaption>Revised L-system tree.</figcaption>
</figure>

Paired with Adam to debug his chess engine, discovered that chess engines are much more complicated than I realised. I'd recently watched [this video](https://www.youtube.com/watch?v=Ne40a5LkK6A) about a chessbot competition, but it didn't take the lid off the complexity of generating all the valid moves and handling the board state efficiently.

Implemented message serialisation in my BitTorrent client, and wrote tests to make sure all the  message types survive a roundtrip (message -> bytes -> message). Now feeling much more confident that it'll work.

### Tuesday, January 30th
BitTorrent client: Wrote code to stitch together a "piece" of data from accumulated network packets, verify its SHA1 hash, and write it to disk. Now ready to start writing the actual client logic, which is the last bit of the puzzle! Hopefully I'll be able to wrap up this project soon.

Felt inspired after attending Zack's SuperCollider workshop. Also had a fun coffee chat with Charlie, where music programming once again raised its enticing head.

While looking up Common Lisp bitwise operators, I randomly came across [this cool article](https://pvk.ca/Blog/2014/03/15/sbcl-the-ultimate-assembly-code-breadboard/) about implementing a Forth using the assembly language DSL (domain specific language) of SBCL (Steel Bank Common Lisp, the most popular implementation of CL). Spotted a fellow Recurser in the comment section!

Finished the day by working on documentation for Sketch and discussing the API of a new feature.

### Wednesday, January 31st
Didn't get much done today due to Real Life™ and once again overestimating my ability to work on public transport. Also had to miss the career workshop, bah. I did get a lot of reading done, however, and I finally feel confident in how to go about implementing a Forth interpreter, so tomorrow maybe I'll actually write some code for that! 

### Thursday, February 1st
Started the day by working on Sketch's [documentation](https://github.com/vydd/sketch/pull/158), because who doesn't love writing docs.

Fixed a bug in how .torrent files are loaded in the BitTorrent client. My goal is to have it downloading files by the halfway point of the batch, which I think would be about the middle of Week 7. Hopefully that's not too ambitious!

Finally got started on writing the Forth interpreter. Right now it's a glorified stack calculator and there's no way of defining/executing new "words", which are basically the Forth equivalent of functions.

    :::lisp
    > (interpret)
    1 1 +
    .
    2

I want to implement the interpreter *in Forth* as much as possible, and it'll take some more reading and thinking to understand the minimal set of primitives I'll need.

Here are some definitions of classic Forth words I've implemented so far, nothing too crazy:

	:::lisp
	(defcode "dup"
	  (fpush (fmemget (1+ *sp*))))
	(defcode "drop"
	  (fpop))
	(defcode "swap"
	  (let ((a (fpop))
			(b (fpop)))
		(fpush a)
		(fpush b)))
	(defcode "."
	  (format t "~a~%" (fpop)))

### Friday, February 2nd
Mostly worked on the BitTorrent client today, now at the point where I have "worker threads" that run in a loop accepting commands from the "brain thread" and reading messages from the network. Once I get that debugged, all I have left is to implement the "brain thread".
