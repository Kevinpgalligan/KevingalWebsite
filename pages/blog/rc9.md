title: Recurse Center, week 9
date: 2024-02-29
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc
tagcount-exclude: y
rss-exclude: y

### Monday, February 26th
Had a jobs chat with Rachel from the RC careers team. She gave me feedback on my CV and had some helpful suggestions on how to brainstorm what sorta jobs I'm looking for.

Wrote a bunch of unit tests for the BitTorrent client, it'll probably take me the rest of the week to test it adequately and iron out all the simple bugs. Hopefully I'm near the finish line?? (Spoiler alert from the future: he was not near the finish line).

Made this animation of a [Reuleaux triangle](https://en.wikipedia.org/wiki/Reuleaux_triangle). It's a type of shape that is the same width in every direction, like a circle, which is why you can rotate it snugly inside a square! I learned about it from a Martin Gardner article. Spent AGES writing trigonometry code to draw the arcs.

<figure>
<video width="300" height="300" class="centered" controls title="A red reuleaux triangle (like a triangle but rounder) rotating inside a square, snugly touching the sides at all times).">
    <source src="{{ url_for('static', filename='video/recurse/reuleaux.mp4') }}" type="video/mp4">
</video>
</figure>

My approach was to take the vector from the center of the arc to the edge and convert it to polar coordinates: `(x,y) -> (r,theta)`. The arc then comes from adding offsets to `theta`. However, I had to battle with Common Lisp's `atan` function to get `theta` from `x` and `y` -- I wanted the angle, in radians, to always be positive, and for it to represent the counterclockwise angle from the positve x-axis, which is not what CL's `atan` gives you. In the end, this turned out to be annoyingly tricky to get right, so I just gave up and accepted whatever it was returning. Anyway, [here](https://github.com/Kevinpgalligan/sketches/blob/master/src/thesketches/reuleaux.lisp) is the code for the drawing.

### Tuesday, February 27th
Wrote more boring unit tests for the BitTorrent client! Considering the complexity of the client logic, it has been surprisingly pleasant to write these unit tests, but it's still time-consuming. [Here's](https://github.com/Kevinpgalligan/cl-bittorrent/blob/master/t/client.lisp) where the tests live.

Had a fun pairing session with Ivy on the Book of Shaders, Chapter 6 (Shapes). Slowly building an intuition for distance fields and how they are used to draw shapes.

Inspired by all the cool Emacs setups I've seen at RC, I devoted some time to messing with my .emacs config. Now using `ivy` / `counsel` for search completion, though I'm not sure that I've configured it properly. Also want to try out fuzzy search, projectile, file trees, etc.

### Wednesday, February 28th
More time on BitTorrent unit tests. Yawn.

Paired with Vedashree on making a programming language mascot for Creative Coding, resulting in this magnificent creation.

<figure>
<video class="centered" controls>
    <source src="{{ url_for('static', filename='video/recurse/giraffe.mp4') }}" type="video/mp4">
</video>
<figcaption>A giraffe acting as the evaluator for a stack-based language. The input, "1 2 +", gets evaluated to "3".</figcaption>
</figure>

### Thursday, February 29th
Today was Impossible Stuff Day, where you're supposed to pick something that seems impossible and do it. I decided that I was going to figure out how assembly language programs call C programs, and vice versa. More generally, I'm interested in how programs actually get executed and how they invoke each other. How does the Python interpreter make calls into C code? How is Lua embedded in other programs? Aaaand so on.

But first, I couldn't resist the urge to finish some unit tests in the afternoon, woops. I also joined the Book of Shaders group for an hour. Now battling with the chapter on transformation matrices.

Don't understand why this cross is wobbly, it's just supposed to be rotating while moving along a curve! It would make more sense to me to apply the transformation to the coordinates of the cross itself, rather than moving the whole space around, but then I'm not sure how rotations would work.

<figure>
<video class="centered" controls title="A cross that's moving along an upside-down quadratic curve while rotating and (for some reason) wobbling.">
    <source src="{{ url_for('static', filename='video/recurse/wobbly-cross.mp4') }}" type="video/mp4">
</video>
<figcaption>A cookie for whoever can tell me why the cross is wobbling.</figcaption>
</figure>

The code:

	:::glsl
	#ifdef GL_ES
	precision mediump float;
	#endif

	#define PI 3.14159265359

	uniform vec2 u_resolution;
	uniform float u_time;

	mat3 rotate2d(float _angle){
		return mat3(cos(_angle),-sin(_angle), 0.0,
					sin(_angle),cos(_angle), 0.0,
					0.0, 0.0, 1.0);
	}

	mat3 translate2d(float dx, float dy) {
		return mat3(1.0, 0.0, 0.0,
					0.0, 1.0, 0.0,
					dx, dy, 1.0);
	}

	float box(in vec2 _st, in vec2 _size){
		_size = vec2(0.5) - _size*0.5;
		vec2 uv = smoothstep(_size,
							_size+vec2(0.001),
							_st);
		uv *= smoothstep(_size,
						_size+vec2(0.001),
						vec2(1.0)-_st);
		return uv.x*uv.y;
	}

	float cross(in vec2 _st, float _size){
		return  box(_st, vec2(_size,_size/4.)) +
				box(_st, vec2(_size/4.,_size));
	}

	void main(){
		vec2 st = gl_FragCoord.xy/u_resolution.xy;
		vec3 color = vec3(0.0);
		float x_offset = mod(u_time,2.0);
		if (x_offset > 1.0) {
			x_offset = 2.-x_offset;
		}
		x_offset -= .5;
		x_offset = smoothstep(-.5,.5,x_offset)-.5;
		float y_offset = pow(st.x-.5, 2.0);
		mat3 transf = translate2d(.5, .5)
			* rotate2d(sin(u_time)*PI)
			* translate2d(-.5, -.5)
			* translate2d(x_offset, y_offset);
			
		vec3 pos = transf * vec3(st, 1.0);
		st.x = pos.x;
		st.y = pos.y;

		color += vec3(cross(st,0.4));

		gl_FragColor = vec4(color,1.0);
	}

What I actually did for Impossible Stuff Day:

1. Found [an article](https://www.devdungeon.com/content/how-mix-c-and-assembly) that shows concise examples of calling C from assembly, and vice versa. To understand this on a deeper level I'll have to learn about: various assembly language things (including how the `call` instruction works), syscalls, executable file formats, linking.
2. Read [this Julia Evans article](https://jvns.ca/blog/2023/08/03/behind--hello-world/) that breaks down roughly what happens when a program is called from the command line. This was a nice way of exploring my unknown unknowns, and showcased some neat tools I hadn't used before like `pstree` (displays a tree of all the processes running on your system), `dd` (copies bytes from disks/filesystems), and `debugfs` (filesystem debugger).
3. Explored a couple of other articles. I think my next step is to work through this [assembly language tutorial](https://asmtutor.com/#lesson1) and then try to write the "hello world" assembly/C polyglot program from scratch!

### Friday, March 1st
Impossible Stuff Day, Part 2: I've gotten as far as I want to in the tutorial on x86-32 assembly. I can make system calls and write FizzBuzz, so now I'm ready to call my assembly program from C!

- This was my first time doing manual syscalls, very cool to talk directly to the OS.
- It was PAINSTAKING to debug, even with GDB. When the program crashes, all you can do is scour your code for mistakes or step through the execution instruction-by-instruction, checking your assumptions at each point. E.g. I was stuck for ages because the `idiv` instruction uses the concatenation of the `edx` register and the `eax` register as the dividend, when I thought it was only using `eax`.

Spent an hour playing around with XMonad, a minimalist window manager. As I said to Reed, this was my first "how do I exit vim" moment in a while! I didn't configure it enough or get comfortable enough to be productive, so for now I'm back to the safety of Cinnamon.

Paired with Sareena on her WebRTC implementation. It was cool to see TypeScript in action and to learn a bit about another network protocol. Interesting parallels with BitTorrent. The central server in WebRTC seems to play a bigger role.
