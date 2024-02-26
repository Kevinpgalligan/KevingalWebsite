title: Recurse Center, week 8
date: 2024-02-23
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc
tagcount-exclude: y
rss-exclude: y

### Monday, February 19th
I didn't have access to my laptop charger, so I had to switch to my desktop for the day. This was an excuse to try out entirely new things.

Had a nice chat with Thomas about Patrick Kavanagh, Barcelona, real life, *Code: The Hidden Language*, comparative literature vs tech, RC vs academia, etc.

Started *The Book of Shaders*! Battled to understand how the author uses `smoothstep` to plot functions and why plotting `exp` was making the whole screen white. Turned out that the values were too large and the curve was basically "off the screen", so I had to scale it down. It really helps me to have a pencil & paper in front of me to figure these things out. Moved on to the colour chapter and made a rainbow.

<figure>
<img src="{{ url_for('static', filename='img/recurse/rainbow.png') }}"
     alt="A simple rainbow with a blue background."
     class="centered">
<figcaption>It's a rainbow!</figcaption>
</figure>

Here's the code:

	:::glsl
	#ifdef GL_ES
	precision mediump float;
	#endif
	vec3 bg = vec3(0.000,0.840,0.900);
	void main() {
		vec2 st = gl_FragCoord.xy/u_resolution.xy;
		vec2 center = vec2(0.5,0.0);
		float dist = pow(st.x-center.x, 2.0)
			+ pow(st.y-center.y,2.0);
		vec3 col;
		if (dist > 0.075 && dist < 0.1) {
			col = vec3(1.000,0.072,0.993);
		} else if (dist > 0.1 && dist < 0.125) {
			col = vec3(1.000,0.102,0.384);
		} else if (dist > 0.125 && dist < 0.15) {
			col = vec3(0.026,0.051,1.000);
		} else if (dist > 0.15 && dist < 0.175) {
			col = vec3(0.044,1.000,0.161);
		} else if (dist > 0.175 && dist < 0.2) {
			col = vec3(1.000,0.975,0.026);
		} else if (dist > 0.2 && dist < 0.225) {
			col = vec3(1.000,0.635,0.000);
		} else if (dist > 0.225 && dist < 0.25) {
			col = vec3(1.000,0.0,0.000);
		} else {
			col = bg;
		}
		gl_FragColor = vec4(col, 1.0);
	}

Tried out the [Hacker101 CTF](https://ctf.hacker101.com/), it's hard! Also, it seems to freeze my browser a lot? Nice to dip my toe in, anyway. The Easy challenges seem to involve inspecting the source code of the page; scanning the site for hidden pages; tweaking POST requests to gain access to resources you're not supposed to; automated guessing of usernames/passwords; and injecting SQL into forms. I'll tag along to the CTF group and see if I can absorb any wisdom.

### Tuesday, February 20th
Read a bunch of [Julia Evans articles](https://jvns.ca/) about TCP/IP, highly recommended! Also found this really helpful [Wireshark tutorial](https://www.youtube.com/watch?v=xdQ9sgpkrX8).

Joined the CTF group, wasn't paying full attention because I was trying to set up a J environment in the background, but I learned about some new tools -- hydra for password guessing, and SecLists for lists of usernames/passwords. Will definitely try more CTF challenges!

Paired with Elias on coding up the [Luhn algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm) in J! The Luhn algorithm is used to check for errors when you enter your bank card number. 

Here's the code! We were trying to make it as "tacit" as possible, which means composing functions and then applying them all at once to the input, and avoiding references to the input itself. The first line of `luhn` is pretty ugly, wondering if there's a better way to do it. Also, surprised by how much J knowledge I've retained.

	:::j
	digitsum =. 10&| + (<.@%)&10
	luhn =: monad define
	  multiplied =. (((1&+) @ (2&|) @ (1&+) @ i. @ $) * |.) y
	  s =. +/ digitsum multiplied
	  10 ([ - |) s
	)

Had a marathon coffee chat with Will. Creative programming vs pragmatic programming, music programming, RC, psychedelics, language learning, etc.

### Wednesday, February 21st
Handed in the printed copies of my thesis! Happy to be done with it, finally. Felt emotional walking around the university grounds, spent 8 years of my life there! Wrote this from an empty lecture hall. My fellow RCers were enthusiastic in their congratulations, which was sweet of them!

Managed to get a BitTorrent client unit test passing, woohoo! This was my Small Goal for the day. Lots more to test, though.

Worked on Chapter 6 (Shapes) of the Book of Shaders. Recreated a Piet Mondrian painting!

<figure>
<img src="{{ url_for('static', filename='img/recurse/piet.png') }}"
     alt="My recreation of a Piet Mondrian painting. It's abstract. 3 rectangles: green, red, blue. A cream-ish background. And it's all overlaid by thick criss-crossing black lines."
     class="centered">
<figcaption>Piet Mondrian, eat your heart out.</figcaption>
</figure>

The code:

	:::glsl
	#ifdef GL_ES
	precision mediump float;
	#endif

	bool isInRect(vec2 st, float x, float y, float width, float height) {
		float pct = 1.0;
		pct *= step(x, st.x);
		pct *= step(y, st.y);
		pct *= 1.0 - step(x+width, st.x); 
		pct *= 1.0 - step(y+height, st.y);
		return pct > 0.0;
	}

	void main(){
		vec2 st = gl_FragCoord.xy/u_resolution.xy;
		if (isInRect(st, 0.18, 0.0, 0.05, 1.0)
			  || isInRect(st, 0.04, 0.7, 0.05, 0.3)
			  || isInRect(st, 0.0, 0.8, 1.0, 0.05)
			  || isInRect(st, 0.0, 0.7, 1.0, 0.05)
			  || isInRect(st, 0.6, 0.0, 0.05, 1.0)
			  || isInRect(st, 0.9, 0.0, 0.05, 1.0)
			  || isInRect(st, 0.18, 0.2, 1.0, 0.05)) {
		} else if (isInRect(st, 0.0, 0.7, 0.2, 0.3)) {
			gl_FragColor = vec4(vec3(0.720,0.045,0.128), 1.0);
		} else if (isInRect(st, 0.9, 0.7, 0.1, 0.3)) {
		   gl_FragColor = vec4(vec3(0.920,0.758,0.000), 1.0);
		} else if (isInRect(st, 0.6, 0.0, 0.4, 0.1)) {
		   gl_FragColor = vec4(vec3(0.063,0.438,0.765), 1.0);
		} else {
			gl_FragColor = vec4(
				vec3(0.900,0.887,0.653), 1.0);
		}
	}

### Thursday, February 22nd
Paired with Raghav on reverse engineering the Lichess network protocol. This was an Impossible Project for me, something I've always wanted to do but that seemed mysterious and difficult. It turned out to be not so bad, though!

The network tab of the browser dev tools allowed us to see what messages were being sent over websockets. The messages were simple JSON strings like `{"t":"move","d":{"u":"g8f6","b":1,"a":1}}` -- not the inscrutable binary blobs I'd been expecting. So we could grab the `WebSocket` object in question using `queryObjects(WebSocket)` in the Chrome JS console, and then run `socket.send("JSON_HERE")` to send a message to Lichess! Not sure how to replicate the effect of `queryObjects` in Firefox.

We experimented with being able to input our moves through the chatbox. Step 1: Add a `MutationObserver` to check for new chat messages being inserted into the DOM, Step 2: Extract move from message using regex, Step 3: Send move to Lichess. As soon as this was ready, I sent a message that caused Raghav's queen to move into my bishop's line of fire, muahaha.

Paired with Charlie on finishing the 903-byte guitar tuner [web app](/apps/tuner.html). It uses the WebAudio API to make sounds, and various code-golfing tricks to get the file size down. I also wrote a short blog post about it, which led down the rabbithole of working on my website. Added "previous" and "next" links to blog posts, and now I'm trying to improve the static website generation so that it doesn't regenerate files that haven't changed, because it was starting to take annoyingly long. Not exactly what I was planning to work on this week, but it needs to be done.

### Friday, February 23rd
Paired with Thomas on reviving my [computer emulator](https://github.com/Kevinpgalligan/hidden-language) project (based on the architecture in *Code: The Hidden Language of Computer Hardware and Software*). We heavily refactored the implementation of logic gates and wrote unit tests for them. It still can't emulate anything, though. Thomas also showed me his very aesthetic Emacs setup, which I can only aspire to!

Spent ages working on my website code so that it doesn't regenerate blog posts if it doesn't need to. Finally done. No more mild annoyance as I wait 5 seconds for the site to compile.

I did a lot of pairing this week. It does take time away from my Big Projects (BitTorrent client & Forth interpreter), but it's more fun than working alone and also more productive (I think). Plus, there's not that much time left to interact with all the cool people at RC (~4.5 weeks), and the community element is something I won't have access to afterwards.

It's now time to polish my CV and start the job hunt. Next week, I'll try to make progress on the BitTorrent client while plugging away at the Book of Shaders. It's funny how I spent hours trying to get a single unit test working when I was working on the client last week. This week, it took me ~10 minutes to get it done. I wasn't stressed or tired this time, and during my time away I'd figured out a better way to write the test. This reinforces the idea that I shouldn't force myself to keep working on something if it's a struggle. Better to take a joyful break and come back refreshed.

I'm happy with the diversity of things I worked on this week, and all the people I got to talk to / pair with. It feels like I'm almost at the point where I've done a little bit of everything from my massive [list of project ideas](/blog/recurse.html#project-ideas). Networking, graphics, programming languages, workflow. Not to mention the many projects that I've experienced vicariously through other people.  My main objectives for the remaining time are to (1) finish my Big Projects, and (2) dig into systems programming and music programming. On the systems side, I want to understand how ELF files work and, more generally, how programs call each other. On the music side, the SuperCollider workshops have been excellent and inspiring, but I haven't actually made any music yet, besides tinkering with the guitar tuner.

The main gap in my plan is that I haven't done any machine learning or data science stuff. Math -- yes, I've gotten my hands dirty with some applied math via computer graphics. But considering that I'm contemplating a career switch to machine learning and data science, this may have been an oversight on my part. Let's see if I can worm my way into a job with what I already know of these subjects.
