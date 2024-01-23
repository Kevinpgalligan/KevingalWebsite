title: Recurse Center, week 3
date: 2024-01-19
description: A weekly log of my activity at the Recurse Center, a 12-week programming retreat.
requires: code
publish: y
tags: rc

### Monday, January 15th
Had a nice chat with Julie, learned a lot about music and music programming! She's doing cool graphics stuff, and shared a bunch of cool links with me. She also remembered my EmojipastaBot that went viral on reddit, which was a blast from the past.

Tried to optimise the [reaction diffusion animation](https://github.com/Kevinpgalligan/sketches/blob/master/src/thesketches/reaction-diffusion.lisp) that Tristan and I made on Friday, which basically consisted of adding type declarations to all the simulation code. It was still pretty slow, though. When I profiled the code, it turned out that the drawing part was the real bottleneck and was taking ~28 times longer than the simulation part! We "manually" draw each pixel as a rectangle, which, under the hood, involves pushing vertices to the GPU. I may look into a faster way of doing it.

<figure>
<img src="{{ url_for('static', filename='img/recurse/reaction.png') }}"
     alt="What kinda looks like a skull & crossbones in black & white."
     class="centered">
<figcaption>A still from the reaction diffusion animation.</figcaption>
</figure>

Joined the TIC-80 tour given by Quinten, it's really cool! Was struck by the idea of writing a Forth compiler with WASM as a target, which could then be run on the TIC-80. First I'd better do a Forth interpreter, though.

Ended the day with a long debugging session, finally tracked down the bug that was causing my `sketch` refactor to break. Though, when I fixed it, it unearthed several more broken behaviours that I'll have to investigate. Tomorrow I'll focus on BitTorrent stuff.

### Tuesday, January 16th
Enjoyed part 2 of the SuperCollider workshop! Was also listening to a cool electronic music playlist that Julie shared with me!

Wrote the skeleton of my BitTorrent client, hopefully I'll be handshaking with peers tomorrow! The only blocker is that most trackers are communicating over UDP, I've only implemented the HTTP tracker protocol, and the HTTP tracker I've been testing against was down today.

Had a coffee chat with Alex, enjoyed hearing about his embedded projects! Looking forward to seeing his networked videogame and model train in action.

I read most of [this article](https://ratfactor.com/forth/the_programming_language_that_writes_itself.html) about Forth, I'm now pretty excited to write a Forth interpreter/compiler! Especially thinking about how Lisp macros can be used to express the Forth words in a DSL, and how code can be shared between an interpreter and a machine code-generating compiler.

### Wednesday, January 17th
Figured out the OpenGL bug in my refactoring of the sketch graphics library. The way I had restructured the main class was causing resources to be created without the OpenGL context.

Paired with Christopher to implement socket-based communication with other BitTorrent peers. I haven't done much byte-shuffling in Common Lisp, and I haven't done socket programming before, so this has been a slow process. I've also discovered that the bytes of an individual peer-to-peer message are not guaranteed to arrive in the same network packet, adding an annoying layer of complexity.

I feel bad that I've been the driver in all of my pairing sessions -- I'm almost at the point of begging the next person to let me be the navigator! 

### Thursday, January 18th
This morning, I finally submitted a [pull request](https://github.com/vydd/sketch/pull/134) to fix a tricky bug in sketch. There are still some small changes required to ensure backwards compatibility, but I'm counting this as a win!!

Today was Impossible Day at RC, where everyone is supposed to attempt something that seems impossible to them. I couldn't participate, though, because I was traveling to Dublin in the evening for a Lisp meetup. Maybe Friday can be my Impossible Day.

Tried to work on the bus but looking down was making me feel ill, ended up napping instead. Did manage to finish reading [this overly-long article](https://ratfactor.com/forth/the_programming_language_that_writes_itself.html) about Forth. Now feeling hyped about writing a Forth interpreter. Also read some Godot articles, inspired by Quinten.

### Friday, January 19th
Read through some [Godot tutorials](https://kidscancode.org/godot_recipes/4.x/) that Quinten shared, excited to try Godot now! Also read about how other languages can interface with the engine, since that seems like it could be a fun project. They have a C API called GDExtension that has been used to, e.g., make Rust bindings. (This was actually at 3am last night when I couldn't sleep, lol).

Reed's enthusiasm for Forth inspired me to shuffle my priorities! Read [this](https://ratfactor.com/forth/the_programming_language_that_writes_itself.html) overly-long but entertaining article, and am enjoying the [jonesforth.S](https://github.com/nornagon/jonesforth/blob/master/jonesforth.S) literate implementation of Forth. I've also started writing a Forth interpreter. The plan is to implement Forth words in the order that they appear in [Starting Forth](https://www.forth.com/starting-forth/).

Coffee chat with Kenneth, talked about shaders and graphics and how to handle motivation at RC.

Tweaked my [pull request](https://github.com/vydd/sketch/pull/134/) for the sketch graphics library, trying to ensure backward compatibility even for users who rely too much on the implementation details.

Making a tactical retreat from the BitTorrent client because my motivation was flagging. I think I was overwhelmed by the combined complexity of the client design, the network protocol, and all the new Common Lisp packages I'm being exposed to. Next week I'll divide the remaining work into tiny chunks and hopefully that will make it feel more approachable.
