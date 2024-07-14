title: Reflections on Recurse Center
date: 2024-06-21
description: Thinking back on my 12-week batch at the Recurse Center.
tags: rc
publish: y

Earlier this year, I spent 12 weeks at a programming retreat called the Recurse Center (RC). I've [written about](/blog/recurse.html) the application process, and I've shared a series of [daily updates](/blog/tag/rc.html) from my time there. Now, I'd like to reflect back on the experience as a whole: what I did, what I didn't do, what went well, and what went not-so-amazingly. Hopefully this will give others a window into my RC experience, so they can compare their experiences to mine and maybe learn from my mistakes!

#### What I did
(Most of these took less than a day to complete, while a few stretched over multiple weeks).

- Various toy graphics applications: a visualisation of the Lorenz attractor; a slice of toast that always lands on the jammy side; a giraffe that executes a stack-based programming language; and randomly-generated trees.
- Contributed a few pull requests to [Sketch](https://github.com/vydd/sketch), the Common Lisp graphics framework. Also contributed some documentation to the Common Lisp Cookbook.
- My biggest project was implementing a [BitTorrent client](https://github.com/Kevinpgalligan/cl-bittorrent). This involved writing a parser for the bencoding format; designing a multi-threaded client for communicating with multiple peers at once; writing the networking code for (de)serialising messages and sending them over sockets; and implementing the actual message-passing and logic for dealing with peers.
- Improved my Emacs workflow, e.g. learned Magit. It's a GAMECHANGER.
- Dabbled in music programming with SuperCollider.
- Worked through several chapters of the Book of Shaders. This was my first significant experience writing shader programs.
- Read a lot about the workings of Forth and got most of the way to implementing a basic [Forth interpreter](https://github.com/Kevinpgalligan/cloroforth) - to be continued!
- Read a few chapters of Learn You a Haskell and learned what a monoid is. (Already forgotten, lol).
- Wrote C code that could call assembly, and vice versa. This revolved entirely around understanding the compilation and linking process, rather than having to write any special code (...which probably should've been less surprising than it was). Also learned a bit about the ELF format for executables.
- Made a [shitty guitar tuner](/apps/tuner.html) in less than 1KB. More precisely, it's 903 bytes of HTML and JavaScript. The audio breaks when I play it on my phone, so I need to figure out how to adjust the playback for different devices.
- Visited the Center for Computing History (and wrote a [blog post](/blog/center-computing.html) about it).
- Revived my unfinished [computer emulator](https://github.com/Kevinpgalligan/hidl), which is based on the book *Code: The Hidden Language*. Just did some refactoring and added unit tests. It's still a long way from working!
- Reverse engineered the Lichess network protocol. Managed to make a piece move by entering a message in the chatbox. Learned about network protocols and WireShark.
- Dipped my toe into some hacking challenges (MicroCorruption, Hacker101 CTF).
- Implemented the Luhn algorithm in J, for validating credit card numbers.
- Made tweaks to my website, like adding previous/next links to blog posts. This has turned out to be more of a pain than I realised, and my implementation still has a bug.
- Began my study of data science, working through a chapter of *Learning from Data* and some of Andrew Ng's lectures.
- Pair-programmed with other people on some of my projects above, and also on their projects. For example: Paxos protocol, chess engine, ray tracer, WebRTC implementation, CryptoPals, an interpreter in Clojure.
- Attended weekly presentations where people showcased their stuff.
- Gave two presentations of my own. One on C-calling-assembly-calling-C, and one on the BitTorrent client.
- Had many "coffee chats" and discussions over Zoom with other Recursers.
- Attended several events that were run by the staff of RC to help us make the most of the experience, e.g. the Building Your Volitional Muscles workshop helps you figure out what you want to work on.
- Attended many Recurser-run events (TIC-80 tour, SuperCollider workshops, Graphics Goblins, ...).

#### What I didn't do
- Make a game.
- Write a machine code-generating compiler.
- Implement an assembler.
- Implement a debugger.
- Learn how to debug network issues (except what I learned from that WireShark tutorial).
- Learn anything about operating systems.
- Try out other operating systems.
- Write a device driver for my piano.
- Experiment with Smalltalk.
- Finish my project to generate RSS feeds for online comics and blogs.
- Finish my webcam theremin.
- Get very far with reverse engineering and hacking.
- Write a BASIC interpreter in assembly.
- ...etc.

#### What went well
*The variety of things* I worked on. My flickering attention span can sometimes be a weakness, but in this case I got a taste for lots of different technical subjects, from hacking to networks to graphics. In turn, this has strengthened my "volitional muscles" (in the RC parlance), as I now have a better idea of which computer science topics I'm most interested in. It was also nice to follow along with the massive diversity of projects that other people were working on.

*Pair programming!* This isn't something I had significant experience with before. It's a great way to get inside someone else's project and to socialise productively!

*The loose structure.* You can work on whatever you want, attend as many events as you want, interact with other people as much as you want. The RC faculty and infrastructure enable you to make RC what you want it to be: Zulip for text chat, a calendar for organising events, Virtual RC/video conferencing for video & audio chats, various faculty-run events, and more! The infrastructure meant I could attend 100% remotely, which of course is excellent from an accessibility perspective.

*The community!* It's probably the most [diverse](https://www.recurse.com/diversity) and welcoming tech space I've been part of. There are [social rules](https://www.recurse.com/social-rules) in place to prevent various misbehaviours that are common in the programming world ("um, actually, this whole thing you did is pointless because blahhhhhi'mupmyownarse"), and they strive to make each batch of attendees as balanced as possible in terms of race and gender. The results speak for themselves -- I met a lot of cool and inspiring people from a wide range of backgrounds, and everyone was super friendly and supportive! To give one example of the friendliness, I handed in my thesis during the batch and was pleasantly surprised to be swarmed by congratulations! It sounds cheesy, but it was a privilege to experience RC alongside these wonderful people. I especially liked meeting those who came to programming through a non-traditional route, whether that was English lit. or music or physics.

*The career services.* RC is a business that makes its money by sourcing good programmers for companies. I like how non-intrusive this aspect is; you can engage with it as much or as little as you want. I should've engaged MORE with their career services, but I at least had my CV reviewed and got some jobhunting tips. In the end, however, RC did lead to me landing a job! A fellow Recurser sent me a link to a job posting that they thought would suit me. I applied, went through several rounds of interviews, and received an offer a few days ago! Woohoo!

There are other things I liked about RC, but I'll just list one more: the *niceties!* People are encouraged to write each other nice messages when a batch is over. And, in the "never graduate" ceremony that closes out the batch, the faculty read aloud a single hand-picked nicety for each Recurser. My batch was the first one where they introduced a portal for sending and receiving niceties, where they will be stored for ETERNITY.

#### What went 'meh (:-/)'
I convinced myself that RC would be a haven of stress-free programming. Unfortunately, while there was no fire-breathing boss breathing down my neck, I still felt quite a lot of pressure... from myself. Partly, this was due to the public accountability mechanisms at RC. The principle of [learning generously](https://www.recurse.com/self-directives#learn-generously) leads most people to share their progress, and I myself posted a daily update in Zulip throughout the batch. In that context, with other people sharing their awesome wins, it could be hard to admit publicly that "today I sucked".

Mostly, however, the pressure derived from the whole thing feeling very personal. I chose to do this programming retreat, there were certain things I wanted to get out of it, and it was a blow to my idea of myself as a competent programmer/human whenever it felt like I was failing to meet those goals. One of the ways that the self-imposed pressure manifested itself was that whenever I encountered a programming roadblock, I felt a strong impulse to skive off and play chess. My instinct was telling me to RUN AWAY rather than sit with the discomfort, with the threat to my ego, and work through the problem. A game of chess is harmless enough, but I usually felt pretty bad about myself when this happened. It's a behaviour I've struggled with since my PhD, and clearly it's something I need to get to the bottom of.

That said, I did manage to get past my programming-related stresses, for the most part! These were a few strategies that helped when I came across a scary, overwhelming problem that made me feel bad:

- Break it down into smaller, less scary chunks.
- Take a break, work on something else.
- Ask for help.

In terms of the projects I worked on, my main regret is sinking so much time into the BitTorrent client. I thought I could blast it out in a week or two, learn some network programming, and then move on to my Real Projects. In reality... it took me almost the entire 12 weeks to finish it. Past a certain point, I was working on the client for the sake of finishing it, rather than having any specific learning goal in mind. I did learn a lot, and leveled up my Common Lisp skillz, but I don't think it was a particularly good Learning Project. I could've picked up the same networking concepts by implementing a simpler protocol, and left more time for other topics. On the other hand, the BitTorrent project did give me a few lessons in persistence. There were moments where I despaired at the seemingly endless amount of work left, but by coming up with smaller tasks and chipping away at them every day, it eventually got done. It was also important to let myself off the hook sometimes and work on Fun New Stuff. I could then return to the BitTorrent client feeling reinvigorated.

So, yeah, I probably should've been more conscious of my learning goals as I went along, and I should've been more ruthless when it came to dropping or switching projects. Another thing: I did the BitTorrent client almost entirely by myself. It would've been nice to take on a more sociable project, like working through CryptoPals with the study group.

It was wonderful that I could attend remotely, but the 5-hour timezone difference was a pain. I found myself sleeping in late, then sitting around waiting for RC office hours to start. I had issues with my sleep schedule, because I usually worked at least until 10pm, at which point my brain was firing on all cylinders and impossible to shut off. Another thing about attending remotely is that it can be harder to make connections with people. It felt like there was a whole social scene I was missing out on (board game evenings! movie nights! fun stuff!) by not being there in person. There were some people I basically never talked to because they didn't have an online presence. So, if/when I return for another batch, I'll probably do it in person.

Zulip is a useful tool, but it's also a reminder of why I shy away from social media. I found myself checking back on my daily check-ins to see who had reacted to them. Whenever there was a lack of engagement, or whenever I compared myself to other recursers who seemed to have made stronger connections, it felt BAD. Embarrassing to admit it, but social media messes with my insecurities. Zulip is also very noisy, notification-wise, which can be overwhelming when you first join. This can be tamed by tweaking the settings, but there's no way to stop yourself getting notified when people tag @Everyone, and so I found myself spammed by email notifications for meetings I didn't want to attend. But this is just a quibble.

I was usually the one to initiate collaborations with other people. This is probably a function of (1) the projects I worked on, (2) remote attendance, and (3) maybe the vibes I gave off?? Whatever the reason, it would've been nice if other people had taken more initiative, but it wasn't really a problem in the end. I didn't mind reaching out first, and I never once got turned down or an unfriendly response from anyone. If I'd really been struggling, I could've relied more on the Zulip pairing bot, which basically acts like a matchmaker for pairing sessions!

I wrote a check-in post every single day, and read EVERY SINGLE check-in posted by my fellow Recursers. Reading check-ins was the first thing I did every morning, and often took upwards of 30 minutes. While it led to some nice interactions that wouldn't have happened otherwise, and was helpful for feeling connected to others from 1000km away, I do wonder if I could've spent that time better. It was mostly a one-way interaction and didn't contribute so much to forming connections. At the very least, I could've experimented more with my routine, and I probably shouldn't have forced myself to read/write check-ins with such fanatical consistency.

#### Final thoughts
The not-totally-awesome section ended up being longer than the super-good-stuff section, but that's only because the critical-sounding stuff was more interesting to think about. And none of the negative stuff implies that there's anything inherently wrong with RC, it's just stuff that I personally found challenging. Overall, I loved my time at RC, and I'd encourage you to [apply](https://www.recurse.com/apply) if you'd like to upgrade your programming skills while surrounded by a really neat group of people. I do think I've become a better programmer as a result of my time there, and being in that environment helped me to learn more about myself, too. Not to mention all the cool people I got to meet, and the job I accidentally got out of it.

#### What I've been up to since then
Despite my intentions to stay in touch with the RC community, I practically disappeared as soon as my batch was over, and I've only peeked at Zulip a handful of times since then. So here's my latest and final (for the time being) check-in.

- 📊 I spent a lot of time preparing a [data science portfolio](/dsml-portfolio.html), because I thought that that was the career path I wanted to pursue. I watched a playlist of Andrew Ng's machine learning lectures on YouTube and implemented various algorithms from it. Ultimately, I felt more excited about the job I've now been offered (programming language designer) than any data science job. It just seems more fun to play around with programming languages than to drown in a sea of data and wait for models to train. That said, hopefully my data science learnings and the portfolio I built up will be useful in the future!
- 🖩 Added a few features to [ka](https://github.com/Kevinpgalligan/ka), my calculator programming language. It was coincidental that this helped me to land the job.
- 🪵 Worked through most of [Learn Prolog Now!](https://www.let.rug.nl/bos/lpn/lpnpage.php?pagetype=html&pageid=lpn-html), with the end goal of being able to write solvers for newspaper puzzles. This has been placed on the backburner for now, but it has greatly increased my appreciation for logic programming!
- 🧮 Started working through Advent of Code 2023 in the J programming language, with the constraint that I'm not allowed to use loops. The goal is to improve my array & functional programming chops. Repo is [here](https://github.com/Kevinpgalligan/advent2023/).

Farewell for now, RC!
