title: A Visit to the Center for Computing History
date: 2024-03-15
description: You should go too!
publish: y
tags: history

I recently had the opportunity to visit the [Center for Computing History](https://www.computinghistory.org.uk/) in Cambridge, England. Since it turned out to be one of my favourite museums ever, I wanted to document the experience and encourage people to go see it if they can. My motive is selfish: I want it to be successful so I can return some day!

The Center has a large collection of computers, from ancient arcade machines, to videogame consoles, to 80s office computers. Most of them are running and interactive. As such, it's accessible and fun even for non-techies.

<figure>
<img src="{{ url_for('static', filename='img/center-computing/outside.jpg') }}"
     alt="The outside of the museum: blue/grey/glass and a sign with the name of the museum."
     class="centered">
<figcaption>When you see "Hello, World", you know you're in the right place.</figcaption>
</figure>

In the lobby, visitors are greeted by a room-filling computer called the [Megaprocessor](https://www.megaprocessor.com/). It was hand-assembled using finger-sized transistors, and the state of each transistor is shown by LEDs. Theoretically, you can watch binary addition unfold before your eyes! I didn't understand the architecture of the computer at all, but it was still cool to see, and you can play Tetris on it.

<figure>
<video class="centered" controls title="The Megaprocessor computer in action, with arcade machine noises in the background.">
    <source src="{{ url_for('static', filename='video/center-computing/megaprocessor.mp4') }}" type="video/mp4">
</video>
<figcaption>Those are noisy arcade machines in the background.</figcaption>
</figure>

In a side room there's a selection of computers from the 80s. The interface for many of them is a keyboard and a BASIC terminal, rather than the graphical user interfaces and mice that we're used to today. I wrote a FizzBuzz-like BASIC program on one of the computers, with the aid of a bulky instruction manual.

<figure>
<img src="{{ url_for('static', filename='img/center-computing/fizzbuzz-program.jpg') }}"
     alt="Fizzbuzz program."
     class="centerfloat">
<img src="{{ url_for('static', filename='img/center-computing/fizzbuzz.jpg') }}"
     alt="And its execution."
     class="centerfloat">
<figcaption>The FizzBuzz(ish) program and its output.</figcaption>
</figure>

I think the monitor is CRT-based, which could be why the screen appears to be faded. If that's the case, my camera picked up on the rapid refreshing of the screen that our puny human brains are too slow to perceive.

<figure>
<img src="{{ url_for('static', filename='img/center-computing/manual-1.jpg') }}"
     alt="The Manual."
     class="centerfloat">
<img src="{{ url_for('static', filename='img/center-computing/manual-2.jpg') }}"
     alt="The Manual."
     class="centerfloat">
<img src="{{ url_for('static', filename='img/center-computing/manual-3.jpg') }}"
     alt="The Manual."
     class="centerfloat">
<img src="{{ url_for('static', filename='img/center-computing/basic-graphics.jpg') }}"
     alt="The Manual."
     class="centerfloat">
<figcaption>The manual. Includes: basics of BASIC, assembly language, music, graphics...</figcaption>
</figure>


Here's a transcription of the BASIC program I wrote:

    :::basic
    10 LET RESULT=0
    20 LET N=1
    30 REPEAT
    40   X=N
    50   Y=3
    60   PROCTEST
    70   IF RESULT=1 THEN PRINT "FIZZ"
    80   Y=5
    90   PROCTEST
    100   IF RESULT=1 THEN PRINT "BUZZ"
    110   N = N + 1
    120 UNTIL N > 20
    130 DEF PROCTEST
    140   LET I=Y
    150   RESULT = 0
    160   REPEAT
    170      IF I=X THEN RESULT=1
    180      I = I + Y
    190    UNTIL I > X
    200 ENDPROC

This BASIC dialect is surprisingly advanced, and allows the definition of procedures & functions (with arguments!). In my ugly implementation, however, everything gets passed around through global variables. Woops. There was an even worse prototype that had GOTOs everywhere.

The development process is interesting. At least, it's interesting for someone who didn't cut their teeth programming at a BASIC REPL. Here are some key points:

- You enter one line of code at a time, with a line number at the start of each line. If you don't include a line number, the code gets executed as soon as you press the return key. Interactive development!
- The command "LIST" prints out all the lines of your program so far, ordered by line number. I'm not sure what happens if your program is too long to fit on the screen.
- "RENUMBER" replaces all the line numbers so that they start at 10 and increase in multiples of 10. This is useful when you're missing a line of code somewhere but there's no space to put it.
- "DELETE m,n" deletes all the lines from number m to number n.
- When the program is ready, you type "RUN".
- I think there's also a command to save the current program to disk.

This computer's version of BASIC has built-in support for graphics and music programming. SuperCollider and p5js, watch out! There was another computer nearby that had a voice synthesizer, which was fun to mess around with.

The final room, and the largest one, is the gallery, which is like an aircraft hangar full of computers, arcade machines, and exhibitions. As in the other rooms, most of the machines are interactive. My favourite part was probably the game consoles, ordered by release date and accompanied by a history of the games industry. All of the consoles were playable! I got to play Mario games on the NES and SNES, Sonic on a Sega, and Crash Bandicoot on a Playstation!

<figure>
<img src="{{ url_for('static', filename='img/center-computing/crash.jpg') }}"
     alt="Crash Bandicoot on a PS1."
     class="centerfloat">
<img src="{{ url_for('static', filename='img/center-computing/mario.jpg') }}"
     alt="Mario on a SNES."
     class="centerfloat">
<img src="{{ url_for('static', filename='img/center-computing/spectrum.jpg') }}"
     alt="Some random game on a ZX Spectrum."
     class="centerfloat">
<img src="{{ url_for('static', filename='img/center-computing/mac.jpg') }}"
     alt="Some Mac computers."
     class="centerfloat">
<figcaption>A selection of consoles and computers from the gallery: Crash Bandicoot on a PS1, Mario on a SNES, a ZX Spectrum, and some Macs</figcaption>
</figure>

There are much older machines on display, but the game consoles resonated the most for me because they've seeped into my consciousness from popular culture.

They have a Mac that belonged to Douglas Adams. They couldn't recover any files from the hard-drive, unfortunately.

They also have the original (clay?) model of the [Creatures](https://www.youtube.com/watch?v=Y-6DzI-krUQ) game world, which was photographed / scanned to create the game assets! (That's a link to an awesome video about the AI in Creatures).

<figure>
<img src="{{ url_for('static', filename='img/center-computing/creatures.jpg') }}"
     alt="The model for the Creatures game world."
     class="centered">
<figcaption>The level map of Creatures!</figcaption>
</figure>

10/10 museum experience. I would highly recommend checking it out if you ever find yourself in that part of the world.
