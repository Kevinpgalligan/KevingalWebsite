title: Recurse Center, week 6
date: 2024-02-09
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc
tagcount-exclude: y

### Monday, February 5th
On the BitTorrent front, worker threads can now handshake and send each other messages over network sockets. Yippee!

To get to that point, I had to refactor my code to work with streams instead of arrays, since that's the interface offered by the usocket library. This involved a frustrating amount of debugging -- I expected that calling `(read-sequence buffer stream)` would read as many bytes as possible into `buffer` until `stream` is exhausted, but instead it quietly hangs if there's not enough data to fill up `buffer`. The solution was to read byte-by-byte, checking `(listen stream)` each time to see if there are any more bytes to read. Even this has a gotcha: if the peer closes the connection after sending some bytes, `(listen stream)` will return `nil` (false), even if there are still unprocessed bytes! I'm willing to accept this wart in my code, however. Credit to gilberth in #lisp for helping me with that.

The next step is to implement the Brain of the client. This will contain all the actual logic for talking to peers, since the worker threads only read and write messages from/to the network.

Other than that, I added a couple of new primitive words to my Forth interpreter, and read through JonesForth trying to figure out what primitives I would need in order to write the interpreter loop in Forth itself. Whoever said that writing a Forth is easy left out "...when you know how to do it".

### Tuesday, February 6th
There are no more tricky design questions to solve on the BitTorrent front, and I just have to write a lot of Boring Code. I think I'm 100-200 lines of  code from a finished client, and then I'm sure there will be 1 million bugs to fix.

I suggested a Forth discussion, and Reed ended up organising (most of) it. Surprisingly, 3 other people turned up, leading to a fun session. We programmed the factorial function in gForth, as a group. Here's my recollection of what we ended up with:

    :::forth
    : fac ( n -- n! )   1 swap 1+ 1 do I * loop ;

And then we had a general chat about Forth stuff. Elias made a perceptive comment about the similarity of Forth and Lisp -- the syntax of both languages maps fairly closely to their representation in memory. Forth is laid out linearly, Lisp is a tree. I wonder if any other languages have this property!

In the evening I had a generative art idea that I couldn't resist implementing: particles that spiral towards the center of the image, leaving a trail of colour. There are a lot of parameters to tweak (how much force is applied to the particles, their max speed, the number of particles and what colour each of them leaves behind, etc), but I'm done playing with it for now.

<figure>
<img src="{{ url_for('static', filename='img/recurse/swirl.png') }}"
     alt="A swirl/spiral generative art pattern, many lines (green, gold, pinkish) spiral towards the center. Green background."
     class="centered">
<figcaption>A swirl!</figcaption>
</figure>

### Wednesday, February 7th
After a coding marathon, the BitTorrent client is now Code Complete™. And the code compiles! I guess I suck at estimating tasks, because it was way more than 100-200 lines of code. My brain is mush and I don't have the heart to run it and see all the runtime errors & protocol bugs, though, so I'll wait until tomorrow before testing it out. I've also been daydreaming about the blog post & presentation I'll probably do to wrap up this project.

### Thursday, February 8th
Read a bit about the [Common Lisp condition system](https://gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html). It's more flexible than try/catch and exceptions because you can control the error behaviour of low-level code from high-level code (e.g. `analyze-log-files` can tell `parse-log-file` to skip malformed entries in the log, or it can tell `parse-log-entry` to return a default value for malformed entries, or it can just not bother to handle the error in the first place).

To test the BitTorrent client, I set up a dummy HTTP server to act as the tracker (which lets you know the IP addresses of everyone who has the file(s) you want to download) and ran the client. This uncovered lots of easy-to-fix bugs. Also added logging so I can understand what the client is doing. For my next and (hopefully) final test,  I'll run 2 instances of the client and try to download a file from myself!

I spent ages trying to debug why my hand-edited .torrent file wasn't loading properly. I correctly suspected that the character encoding might be to blame, i.e. vim messing things up by trying to interpret the file as UTF-8. The solution was to use `vim -b` so that the .torrent file would be treated as a binary file.

Paired with Jake to implement anti-aliasing in his ray tracer. Takeaways: pairing is fun, C is fun, vim can basically be made into an IDE, and .ppm is a cool image format.

Attended the presentations. They were super interesting, as always. I liked how Dan applied CS theory to the mundane problem of hiking route planning: first, it was framed as the traveling salesman problem, and then as a subset cover problem. The ROM hacking presentation was also very fun, and Raunak's WASM interpreter was impressive. I need to start taking notes -- too much cool stuff to remember! After that was the graduation ceremony. Seven-ish days ago, we were asked to write nice things about graduating Recursers. These nice things were now read out, 1 per person. So sweet! I realised that I hadn't put enough thought into what I wrote about other people. I'll try harder next time!

### Friday, February 9th
Worked on testing the BitTorrent client. The idea is to run 2 instances of the client, one with a copy of the data and one without, and download a file from myself. This test is mostly ready, but I've run into a bug where my dummy tracker server is sometimes rejecting my HTTP requests depending on the random 20-byte ID I use. Probably related to character encoding, as usual.

### General note
Further RC updates will not be included in my RSS feed, and will not be included in the tag count, because I don't want people to be spammed by what is essentially my diary. I'll still post the updates every week, however.
