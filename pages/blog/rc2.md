title: Recurse Center, week 2
date: 2024-01-12
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc

### Monday, January 8th
Finally read the rest of the BitTorrent [specification](http://bittorrent.org/beps/bep_0003.html). Apparently, there's a better, [unofficial document](http://wiki.theory.org/BitTorrentSpecification), but I wanted to start from the original.

My understanding is that there are two basic entities considered in the BitTorrent protocol: peers and trackers. Peers download and share files, while trackers keep a list of all the peers. It seems that the next step after finishing my bencoding implementation (the text-based encoding scheme used by BitTorrent) will be to write a client that can communicate with a tracker and fetch the list of peers.

First, though, I have two tasks to work on:

* Take a look at sample BitTorrent traffic in Wireshark, and also peek at some .torrent files. There's a sample network traffic dump [here](https://wiki.wireshark.org/BitTorrent).
* Add unit tests for my bencoding implementation.

By the way, I'm using [this article](http://www.kristenwidman.com/blog/33/how-to-write-a-bittorrent-client-part-1/) as a rough guide.

After reading the Bittorrent spec, I joined the daily check-in meeting. Afterwards, had a chat with Sahil, Reed, and Vedashree about a data-modeling task that Vedashree is working on. Good to brush up on my intuitions about SQL and relational databases!

Later: separate "coffee" chats (I don't drink coffee) with Raghav and Régis. Raghav has an interesting background! He learned how to program through contributing to private Runescape servers, which were made by reverse engineering the Runescape network protocol and recreating the necessary serverside code. He likes Clojure, hopefully I'll get to pair with him on a Lisp thing at some point. Régis is focused on machine learning projects, he gave me some nice suggestions about learning resources -- the homework exercises in Andrew Ng's course might be too easy, apparently.

We had the Building Your Volitional Muscles workshop today, which was basically about the skill of figuring out what you really want to work on. We did an exercise where we wrote 3 columns: project ideas, why we wanted to do them, and which projects we were actually going to do. It was interesting to dig into my motivations, which ranged from "I need this piece of software" to "seems fun" to "this will probably be useful for my future job". Ultimately, I think I'll place more emphasis on the FUN! part, though there aren't many ideas on my project list that wouldn't be at least a little bit fun.

I'd already implemented bdecoding (string -> data), but when it came to actual bencoding (data -> string) I ran into a tricky implementation detail. I needed to decide how to represent the dict data structure (which, when encoded, look like "d4:blah4:spame"). First I was going to use regular lists to represent bencoding lists, and "alists" (Lispspeak for lists of pairs) to represent dicts, but it would be difficult to distinguish between them, since they're both lists. I then considered Common Lisp's `hash-table` type, but it doesn't maintain keys in insertion order, and the alphabetical order of bencoded dicts must be maintained. Finally, I settled on using binary search trees from the [trees](https://github.com/froydnj/trees) package.

Next, with a complete bencoding/bdecoding implementation in hand, it was time to write unit tests, because I was in no way confident that my implementation was correct. It was fairly straightforward to set up the [fiveam](https://lispcookbook.github.io/cl-cookbook/testing.html) test framework. The tests consist of either decoding a bencoded string and making sure the original string is recovered when it's bencoded again, or passing invalid data and making sure it errors out.

Here's what fiveam unit tests look like!

    :::lisp
    (test single-digit-benteger (is-reencodable "i1e"))
    (test multi-digit-benteger (is-reencodable "i12e"))
    (test zero-benteger (is-reencodable "i0e"))
    (test minus-zero (signals error (bencode:bdecode "i-0e")))

Another tricky implementation detail. When I tried to parse an actual .torrent file, I ran into trouble with character encodings. I couldn't even read the .torrent file as a string, because my Common Lisp implementation, by default, tries to interpret the file as UTF-8, when in fact a bencoded file consists mostly of ASCII but also sequences of raw bytes (such as raw SHA1 hashes). Basically, there were sequences of bytes that didn't correspond to any Unicode codepoint as represented by UTF-8. I couldn't load the file as ASCII, either, because it contains bytes outside the ASCII range (0-127). Spent a while spinning my wheels on this, eventually turned to the wonderful folks at the #lisp IRC, who suggested to decode using the ISO-8859-1 character encoding, which assigns a character to each of the 256 possible values of a byte. And it's compatible with ASCII. This suggestion worked, and I was finally able to parse the file. Thanks #lisp!

### Tuesday, January 9th
The timezone difference is challenging. By the time I finish for the day at 10pm, my brain is frazzled and I find it hard to go to sleep.

Joined the daily check-in call for the 2nd day in a row. It's nice to see people and hear what they're working on, though it does evoke dread-filled memories of the daily standups at my old job.

Discovered that Emacs's `evil-mode` (vi bindings) comes with interactive replacement enabled by default, which is very neat! It was under my nose all this time. Thanks to Detective Reed for figuring that out. To scratch my "Emacs customisation" itch, I later spent some time setting up Magit, a git interface for Emacs. I think this will finally enable me to do Lisp programming entirely from Emacs, instead of switching back and forth between Emacs and the terminal. It also appears to be a more pleasant interface than the git CLI.

Had a nice coffee chat with Neda, courtesy of the chat-bot. Yet another person with an interesting background -- she majored in mathematical physics (or maths and physics?) with a minor in software engineering, eventually got a job as a backend software developer. She worked through Crafting Interpreters for most of her batch (should've asked her more about that), now she's planning to grind Leetcode for job-hunting, which could be fun to pair on.

Attended the SuperCollider workshop run by Zack, it made me really excited about music programming! Also attended the Emulators + VMs group, cool work being done there with WebAssembly and various CPU/console emulators. Finally, attended the Graphics Goblins group, where, as you might expect, there were cool graphics projects, including shadows in a terrain generation program, and a mosaic shader. Mostly a day of talking to people and seeing what they're working on. Tomorrow I'll try to focus on programming.

### Wednesday, January 10th
Skipping the daily check-in today. While it's nice to see people, and it provides the opportunity for serendipitous interactions, it feels too much like my old job!! Goodbye daily structure, hello chaos. For accountability, I'll continue doing this weekly blog, and might post my written updates in the #checkins Zulip channel. Still experimenting with my routine, though.

I haven't done any actual network programming so far, despite that ostensibly being the entire reason behind implementing a BitTorrent client. This changes today!

Trackers are HTTP/HTTPS services, they provide information through HTTP GET requests about all the available peers. Those GET requests require various query parameters, e.g. "https://mytracker.com:1337/announce?info_hash=blah&uploaded=0".

I coded up a function to send a GET request with all the appropriate parameters. This involved various libraries: [dexador](https://github.com/fukamachi/dexador) for HTTP requests `(dex:get "https://tracker.com")`, [quri](https://github.com/fukamachi/quri) for URL manipulation (it also handles the percent encoding of query parameters), and [sha1](https://github.com/clibs/sha1) for computing hashes.

I held my breath as I fired off a request to a tracker, only to get back this bencoded message:

    d14:failure reason25:provided invalid infohashe

Outbreath. It was too much to expect everything to work on the first try. The hash seems to match the one reported in qBitTorrent, though! I'll debug it tomorrow.

Attended the Creative Coding session, where we were given the prompt "Murphy's Law". My first thought was to make an animation of bouncing toast that only ever lands on the side with jam on it. Progress so far:

<figure>
<video width="300" height="300" class="centered" controls>
    <source src="{{ url_for('static', filename='video/recurse/buggy-toast.mp4') }}" type="video/mp4">
</video>
<figcaption>My buggy toast animation! Includes a crappy, bug-ridden implementation of Verlet physics based on a Coding Train video I watched a while ago.</figcaption>
</figure>

This ended up consuming the rest of my day, along with a coffee chat with Jake, who did physics in academia and then moved to data science / ML in industry. He wants to learn low-level computer stuff while at RC. Interesting to hear his perspective, since I'm considering a data science career path myself.

### Thursday, January 11th
The toast now bounces, it doesn't stretch, and there's a mysterious force (something to do with a person called Murphy) that always makes it land on the jammy side.

<figure>
<video width="530" height="300" class="centered" controls>
    <source src="{{ url_for('static', filename='video/recurse/toast.mp4') }}" type="video/mp4">
</video>
<figcaption>Murphy's Law and toast.</figcaption>
</figure>

Submitted a [pull request](https://github.com/LispCookbook/cl-cookbook/pull/519) to the Common Lisp Cookbook, adding a description of how to load arbitrary bytes into a string with the SBCL implementation. The Common Lisp community is relatively small, so I think it's important to contribute to documentation efforts like the Cookbook. I'm normally too lazy to do this, but trying to form good habits!

Figured out that quri, the URL library, was inserting extra percent-encoded characters into the info hash, because it was re-encoding the strings as UTF-8. There's an option in quri to set the character encoding of the query string, but this is not exposed in the `make-uri` interface -- hence, [another pull request](https://github.com/fukamachi/quri/pull/85). Yay for yak-shaving! (Also, ended up closing the pull request because there's another way to do it).

After this was fixed, I started getting 400 Bad Request responses from the server. Yet, when I copied the URL (including query parameters) into my web browser, it returned the list of peers! Likewise, when I copied the URL into a 1-line Python script that made a GET request using the `requests` package...

    :::python
    import requests
    print(requests.get("url-here").content)

...it also returned a 200 and the peers list! Now the weirdest part: when I copy/pasted the URL into `(dex:get "url-here")`, instead of doing `(dex:get (quri:make-uri ...))`, it worked! So my ugly fix was to convert the URI object to a string *before* passing it to `dex:get`. Maybe it's a bug in how dexador handles URI objects?

The final task was to parse the list of peers, since it's stored in a binary format. Common Lisp doesn't have great built-in support for parsing binary formats, so I had to spend a while playing around with various libraries. Actually, there's an older format for the peers list I still have to add support for, which uses the bencoding format rather than a bytestring. Besides that, I'm ready to start peer-to-peer communication!

There was a 1-hour session today where people presented what they're working on. I'll have to think of something to share with the group. Maybe something about Lisp macros, or generative art stuff!

### Friday, January 12th
Started refactoring [sketch](https://github.com/vydd/sketch) to change how the SDL2 window is handled. Currently, the main class in sketch inherits from an SDL2 window class. When creating an instance, the window gets initialized first with default dimensions, then the attributes of the sketch class are initialized, and then those attributes (like width and height) are assigned to the window. This causes the window to resize, and anything that's drawn in the first round of rendering is consistently getting lost as a result. The plan is to switch from inheritance to composition: store the window as a slot in the sketch class so that it can be created with the right dimensions from the start. This also helps to loosen the dependency on SDL2 in case we want to add a different backend in the future. It's not the most exciting work, but this bug has been annoying me for a while now.

Stuck on an error with this (partial) stack trace, pinged the other sketch developers for help.

	The value
	  -402557904
	is not of type
	  SB-INT:INDEX
	   [Condition of type TYPE-ERROR]

	Backtrace:
	  0: (CL-OPENGL:GET-SHADER-INFO-LOG 0)
	  1: (KIT.GL.SHADER:COMPILE-AND-LINK-PROGRAM NIL :VERTEX-SHADER " ..)
	  2: (KIT.GL.SHADER::PROCESS-SOURCE #<KIT.GL.SHADER::SHADER-DICTIONARY-DEFINITION {1006829433}> #<KIT.GL.SHADER::PROGRAM-SOURCE :FILL-SHADER> #<KIT.GL.SHADER:PROGRAM {1003009FB3}>)
	  3: ((:METHOD KIT.GL.SHADER:COMPILE-SHADER-DICTIONARY (KIT.GL.SHADER::SHADER-DICTIONARY-DEFINITION)) #<KIT.GL.SHADER::SHADER-DICTIONARY-DEFINITION {1006829433}>) [fast-method]
	  4: (INITIALIZE-ENVIRONMENT #<TESTER {1002405A13}>)
	  5: ((:METHOD INITIALIZE-INSTANCE :AFTER (SKETCH)) #<TES

Spent a while thinking through the design of the peer-to-peer part of my BitTorrent client, and asked in the #networking channel on Zulip for feedback. Here's the preliminary design I came up with:

* A master thread accepts requests to download a torrent, fetches the list of peers, and spins up threads to communicate with those peers.
* Another thread monitors for attempts to connect from new peers and spins up threads to deal with them.
* Once they're created, the threads that communicate with peers are completely independent, sending TCP messages back and forth. They hand off any data received to...
* The final type of thread, which accepts data from peers in a queue data structure and writes it to disk.

The only thing I'm unsure of is how to co-ordinate the threads. Don't want to download the same piece of a file from multiple peers! So maybe there needs to be another type of thread that maintains a list of which peers have which block of data, and peer-threads query it to decide which block they should request next. It's fun to think about this! I think I'll capture the design in a diagram when it's further along.

Wesley suggested storing all the state in a data structure and access it using a lock/mutex, rather than passing everything through queues. The main bottleneck will be the network, so there wouldn't likely be much contention for a mutex. Julian and Wesley also suggested writing directly to disk in the peer-threads, possibly using an async I/O interface like io_uring (which I never heard of before). Network transfer is much slower than disk access, so maybe the peer-threads can handle the disk writes themselves.

Attended the feelings check-in. Everyone there was so open and emotionally intelligent! ✨ I'm not sure what the protocol is for sharing what was discussed in the meeting, so... moving on.

Later, I had a 2-/3-hour pairing session with Tristan, courtesy of the pairing bot. We worked on an animation of the [reaction diffusion](https://thecodingtrain.com/challenges/13-reaction-diffusion) model. We managed to finish it, but the animation is currently a bit slow, so I'm going to optimise the program before sharing a video. Tristan and I talked about Common Lisp, the multi-threading design of my BitTorrent client, and his Rust compression algorithm project. I really enjoyed the experience! I think pairing with someone else really helps me to stay focused, as it forces me to not get distracted by doing dopamine-rich activities like playing chess.

Next week, my goals are: (1) pair on more things, (2) get peer-to-peer communication working, and (3) finish fixing that bug in sketch. I think I want to pivot to another topic after the BitTorrent project is finished, like computer graphics or programming languages, which have more people working on them. It would be nice to try learning as part of a group.
