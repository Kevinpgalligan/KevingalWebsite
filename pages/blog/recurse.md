title: A Recursive Christmas
date: 2023-12-28
description: My experience of applying for the Recurse Center (twice), and what I'm planning to work on there.
publish: y
tags: rc

I'm happy to report that this January I'll be starting a 3-month programming retreat at the [Recurse Center](https://www.recurse.com/) (RC)! It's basically like a writing retreat, but for programming. Since I got rejected the first time I applied, and I found it helpful when preparing my second application to read other people's experiences, I figured I'd do the same thing here. I'll also list the [project ideas](#project-ideas) I have for my batch, which I find VERY EXCITING to think about.

By the way, in the spirit of Christmas, here's a Palestine-themed animation I made of nested Christmas trees. Source code [here](https://github.com/Kevinpgalligan/sketches/blob/master/src/thesketches/xmas.lisp).

<video width="200" height="200" class="centered" controls>
    <source src="{{ url_for('static', filename='video/recurse/xmas.mp4') }}" type="video/mp4">
</video>

### First Application
There are three stages to the RC application process: a written application, a conversational interview, and a pair programming interview. The written application is meant to determine your motivations and your programming background. I felt like my attempt at the written application was pretty strong, and indeed, I managed to pass that stage of the interview. I'll share my final written application in the next section.

The conversational interview is where I got rejected. They don't provide feedback after the interview, but I can guess what went wrong. My motivations for attending RC were not clear or well thought-out. I said I liked the structure provided by RC, and that I thought it would help me to stay motivated -- but the RC website specifically says that they're looking for self-guided and self-motivated people! I also thought the interviewer would appreciate my openness when I shared my one worry about attending RC and said that, in the aftermath of my PhD experience, I sometimes find difficult programming to be anxiety-inducing, which makes me procrastinate and avoid it. In retrospect, I shouldn't have been so self-deprecating. An empathetic interviewer might even have rejected me for the good of my mental health!

Ultimately, I don't know why I got rejected, and I can only speculate. Needless to say, I was gutted, because I'd been fantasising about attending RC for many years. The RC website says that 6% of attendees get in on their 2nd (or later) attempt, which is supposed to be encouraging, but seems like a pretty small percentage to me??? Still, I was determined to try again, and self-reflection had at least given me an idea of how to improve.

### Second Application
I did a crapload of research for my second application. I combed through the RC website, and read upwards of 20 articles and blog posts by people who'd applied to RC. I used these sources for inspiration and to better prepare myself. I also seriously reflected on my motivations, and concluded that the reason I wanted to attend RC specifically was for the community it provides. Anyone can go into the mountains and do a programming retreat by themselves, but it wouldn't be half as fun or as beneficial as working with other people!

[Here](https://gist.github.com/Kevinpgalligan/e03091ff2536f436c2fb56c16b6cdea1) is my second written application, or something very close to it! Besides revising my motivations, I also tried to be more detailed about what I wanted to work on, and what types of projects I'd worked on in the past.

The conversational interview was shorter this time, down from 30 minutes to 15 minutes. It also felt way less intense than the first one. I was prepared for all the questions, since either they were taken from the written application or I'd encountered them during my preparations. The questions were:

* What's your programming background?
* What do you want to work on at RC?
* What was a programming problem you encountered, and how did you resolve it?
* How will you tell that your batch has been a success?

At the end, I had the opportunity to ask my own questions, so I asked the interviewer what their experience at RC had been; how people divide their time between programming and interacting with others; and how flexible RC is in terms of people attending remotely (answer: 100% flexible).

I managed not to self-deprecate this time. The interviewer was really friendly, and I think there was a lot of crossover between what they'd worked on and what I wanted to work on. Also, I think I conveyed my enthusiasm and excitement for RC a lot better this time, since I remember smiling a lot (out of genuine enthusiasm) when the interviewer talked about their experience. Despite these positive signs, I had gotten good vibes from the interviewer during the first application, so my heart rate still doubled when I received a follow-up email from RC, to the extent that I could hear my heartbeat pounding in my ears. I hid in the bathroom to open the email, just in case I burst into tears. I was relieved to find, however, that I had passed the conversational interview this time!

Next up was the pair programming interview. Beforehand, you're required to write a short program. During the interview, you then pair up with your interviewer to add a small feature to that program. I chose to write a toy version of Space Invaders, where the aliens were circles and the player was a rectangle. I also prepared by skimming [this](https://martinfowler.com/articles/on-pair-programming.html) article on pair programming. At the start of the interview, I spent a lot of time talking through my program from top to bottom. Maybe too much time, but the interviewer gave the go-ahead and could've interrupted me if I was taking too long. We then worked on adding the ability to shoot bullets, just about managing to complete the collision-detection logic before running out of time. The point of this interview isn't to complete the task, but to find out what you're like to work with, so I wasn't worried about how much progress I'd made. My approach was basically to talk through every single thing I was doing and to regularly prompt the interviewer for feedback. In practice, this meant I ended a lot of sentences by saying "if that makes sense" or "if that sounds good to you".

I somehow ended up enjoying this interview a lot, despite being tired and sick with a headcold. The programming went smoothly, the interviewer was nice, and they were helpful in catching a bug or two. It is, in fact, fun to program with other people! Still, the pounding in my ears returned when I received the follow-up email today, and I once again felt a wave of relief and happiness when it turned out to be good news.

The whole process took 1 week. My batch starts on January 3rd. For accountability, I'm going to post a weekly update, so stay tuned! I'm looking forward to meeting cool programmers and to 12 weeks of (hopefully) anxiety-free programming!

### Project Ideas
Below are all the project ideas I have, grouped by the following categories: networking, systems, graphics, languages, software, workflow, music, computer science fundamentals, hacking, and maths & machine learning. (Yes, I have too many interests). I've compiled them over many years, from many different sources -- including my brain.

There's no way I'll complete even half of them, so my plan is to do whatever makes me feel most excited on any particular day. The projects I'm planning to start with are marked with a star emoji ⭐. If you have any suggestions or want to team up with me on any of these ideas, please let me know!

##### Networking
* ⭐ Implement a BitTorrent client.
    * <http://wiki.theory.org/BitTorrentSpecification>
    * <http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-1/>
    * <http://www.bittorrent.org/beps/bep_0003.html>
* Network traffic visualiser.
* Reverse engineer a network protocol, e.g. for a web-based game.
* Learn how to troubleshoot a network connection with dig, ping and traceroute.
* Trace ping, traceroute, netstat, DHCP, DNS, ARP, SSL (SSLsniff & SSLstrip).
* Implement a command-line SMTP mailer.
* Play around with DNS, maybe implement it? (and see: <https://www.dns.toys/>).
* Implement a program to spy on local WiFi.
* More project ideas: FTP-based file transfer program; a bandwidth monitor; a port scanner; a mail checker (enter POP or IMAP protocol, web server & IP, and application checks for mail); IP -> country; whois based on IP.

##### Systems
* Reroute microphone input to another computer across the network.
* Call a C function using assembly, and vice versa.
* Device driver for my electronic piano.
* C extension for higher level language.
* Annotate the assembly output of a Hello World C program.
* Hand-write an ELF binary file (inspired by: <https://blinry.org/rc-month-1/>).
* Implement an assembler/linker from scratch (also <https://blinry.org/rc-month-1/>).
* Parser for ELF file format.
* Implement a debugger.
* Print "hello world" during the boot process.
* Try out alternative operating systems (Plan9, Free BSD).
* Operating Systems books.
    * Operating System Design: The Xinu Approach.
    * The Design and Implementation of the FreeBSD Operating System.
* Learn strace: <https://jvns.ca/blog/2021/04/03/what-problems-do-people-solve-with-strace/>
* Create my own filesystem (whatever that means).

##### Graphics
* ⭐ Implement various generative art ideas, see: The Coding Train.
* ⭐ Add features to the sketch generative art framework (<https://github.com/vydd/sketch>).
* 3d renderer using 2d graphics primitives (draw skyscrapers).
* Implement a rasteriser.
* Learn about shaders: shadertoy and Book of Shaders.
* Write Common Lisp bindings for Godot game engine.
* Make a game (e.g. chessbots, players program each type of piece using a DSL).

##### Languages
* ⭐ Implement a Forth interpreter in Lisp.
* Implement a Forth compiler:
    * <https://github.com/nornagon/jonesforth/blob/master/jonesforth.S>
    * <https://ratfactor.com/forth/forth_talk_2023.html>
* Implement a machine code-generating compiler (book: Engineering a Compiler).
* Read: On Lisp (macros).
* Read: The Common Lisp Condition System.
* Read: Lisp in Small Pieces (advanced implementation of Lisps).
* Play around with: Smalltalk (<http://wiki.squeak.org/squeak/377>).
* Use J/APL for mathsy stuff.
* Other languages of interest: OCaml, Guile, Prolog.

##### Software
* ⭐ Finish my [RSS feed generator](https://github.com/Kevinpgalligan/RewindRSS).
* Update my site generator to not regenerate pages that haven't changed.

##### Workflow
* Try out the Nyxt browser.
* Learn Magit for Emacs.
* Write an Emacs plugin.

##### Music
* ⭐ Finish webcam theremin.
* Read: "Theory and Practice of Electronic Music".

##### Hacking
* Maybe useful: <https://github.com/hackclub/some-assembly-required>.
* Read: "Metasploit: The Penetration Tester's Guide"
* Do: <https://cryptopals.com/>
* Do: <https://github.com/guyinatuxedo/nightmare>
* Do: <https://overthewire.org/wargames/bandit/>
* CTF challenges.

##### Computer science fundamentals
* ⭐ Finish computer emulator based on Code: The Hidden Language of Computer Hardware and Software.
* Write BASIC interpreter in assembly (for emulator?).
* Read Paradigms of Artificial Intelligence Programming (algorithms in Common Lisp).

##### Maths & machine learning
* Book: Learning From Data.
* Andrew Ng's machine learning lectures.
* Reinforcement Learning: An Introduction (a book that uses Lisp!).
* Code up stuff from Martin Gardner articles.
* Implement principal component analysis.
* Implement BCH and other error correction codes.
* Read: How to Solve It.
* Implement Fourier transform algorithm.
* Do more Project Euler problems.
