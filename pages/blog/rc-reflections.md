title: Reflections on the Recurse Center
date: 2024-11-21
description: A long-overdue recap of my 12-week batch at the Recurse Center.
tags: rc
publish: y

Earlier this year, I spent 12 weeks at a programming retreat called the Recurse Center (RC). I wrote about the [application process](/blog/recurse.html), and I shared a series of [daily updates](/blog/tag/rc.html) from my time there. Now, I'd like to write what they playfully call a "return statement" and reflect on the experience as a whole[^return]. We'll cover the highs, the lows, and the minutiae of life at RC.

But first, here's a list of everything I did during my batch. Certain projects, like the BitTorrent client, spanned almost the entire 12 weeks, but most were mini-projects. Micro-projects, even!

- Made several toy graphics applications: a visualisation of the Lorenz attractor, a slice of toast that always lands on the jammy side, a giraffe that executes a stack-based programming language, and randomly-generated trees.
- Contributed a few pull requests to [Sketch](https://github.com/vydd/sketch), the Common Lisp graphics framework. Also contributed some documentation to the Common Lisp Cookbook.
- Made my own [BitTorrent client](https://github.com/Kevinpgalligan/cl-bittorrent). This involved a parser for the bencoding format, a multi-threaded client for communicating with multiple peers at once, networking code for (de)serialising messages and sending them over sockets, and the actual logic for dealing with peers. The final result was an unoptimised, fragile mess, but I was able to transfer a file to myself!
- Improved my Emacs workflow. (Thanks to Reed, Thomas, and others. Magit is AWESOME).
- Tried music programming with SuperCollider, and attended the SuperCollider workshops run by Zack.
- Worked through several chapters of the Book of Shaders. This was my first significant experience writing shader programs.
- Read a lot about the workings of Forth and got part of the way to implementing a basic [Forth interpreter](https://github.com/Kevinpgalligan/cloroforth) - to be continued!
- Read a few chapters of Learn You a Haskell and learned what a monoid is. (Already forgotten).
- Wrote C code that could call assembly, and vice versa. To my surprise, this was moreso about understanding the compilation and linking process, than needing to write any particularly fancy code. Also learned a bit about the ELF format for executables.
- Made a [shitty guitar tuner](/apps/tuner.html) in less than 1KB, with the help of Charlie. More precisely, it's 903 bytes of HTML and JavaScript. The audio breaks when I play it on my phone, so I need to figure out how to adjust the gain for different devices.
- Visited the Center for Computing History and wrote a [blog post](/blog/center-computing.html) about it.
- Paired with Thomas on refactoring my barely-started [computer emulator](https://github.com/Kevinpgalligan/hidl) project, which is based on the book *Code: The Hidden Language of Computer Hardware and Software*.
- Figured out the Lichess network protocol with Raghav. (JSON-encoded messages sent over websockets). We hooked into the DOM and managed to make a chess piece move by entering a message in the chatbox. I also learned a bit about WireShark and TCP.
- Dipped my toe into some hacking challenges (MicroCorruption, Hacker101 CTF).
- Paired with Elias to implement the Luhn algorithm in J, for validating credit card numbers.
- Made tweaks to my website, like adding previous/next links to blog posts. This has turned out to be more of a pain than I realised, and my implementation still has a bug.
- Began my study of data science, working through a chapter of *Learning from Data* and some of Andrew Ng's lectures.
- Pair-programmed with many other people not mentioned above. Examples of their projects: a Paxos protocol implementation, a chess engine, a ray tracer, a WebRTC implementation, CryptoPals, and an interpreter in Clojure.
- Attended weekly project presentations, and gave two presentations of my own: one on C-calling-assembly-calling-C, and one on the BitTorrent client.
- Had many "coffee chats" and discussions over Zoom with other Recursers. Also attended many Recurser-run events: a TIC-80 tour, the SuperCollider workshops, Graphics Goblins, ...
- Attended RC-run events that were intended to help us make the most of the experience, e.g. the Building Your Volitional Muscles workshop helps you figure out what to work on.
- There were also quite a few non-programming-related events, like Will's burnout talk, the feelings check-ins, and "any topic is allowed" presentations (can't remember what those were called).

If you read [this post](/blog/recurse.html) from just before I started RC, you'll notice that I didn't get around to implementing even a fraction of the ideas I'd brainstormed. I didn't make a game, write a machine code-generating compiler, or learn anything about operating systems, to name a few projects I missed. In any case, I'm happy with the variety of things I worked on. I got a taste for lots of different technical subjects that I'd always wanted to try. (Hacking! Network programming! Graphics!). I also enjoyed following along with the massive range of projects that other people were working on.

The staff have done a great job cultivating a pleasant environment at RC. It's probably the most welcoming tech space I've ever been part of. Every time I reached out to one of my batchmates, I got a friendly response. They were an interesting group of people, coming from backgrounds as diverse as English literature, music and physics. But most importantly, they were kind!

Pair programming is highly encouraged at RC. It's a great way to spend time with people, learn from them, and get a taste for their projects. It helped to take the initiative in asking people to pair or to chat; if I hadn't done so, then I probably would've spent most of the batch working alone! Another RC tradition is the "niceties". People are encouraged to write each other nice messages when each batch ends. Then, in the "never graduate" ceremony, the faculty read aloud a single hand-picked nicety for each Recurser. It's a cute way to end the experience. My batch was the first one where they introduced a portal for sending and receiving niceties, where the niceties will be stored *for eternity*.

As for the structure of the retreat, we were mostly left to figure things out for ourselves. The RC faculty and infrastructure enable you to make RC what you want it to be, though it's strongly encouraged to work during the office hours and to [Learn Generously](https://www.recurse.com/self-directives). You can work on whatever you want, attend as many events as you want, and interact with other people as much as you want. Helpful stuff includes: Zulip for text chat (with its excellent selection of emojis 🐙), a calendar for organising events, Virtual RC/conference rooms for video & audio chats, staff-run events, and more!

The infrastructure meant I could attend 100% remotely - yay for accessibility! The downside was that it was harder to connect with other attendees. There was a whole world of social opportunities I missed out on, like in-person events, and there were people I barely talked to because they weren't engaged in the Virtual side of RC. It also kinda sucked to be in a timezone 5 hours ahead of the RC hub in New York. I often finished programming after 10pm, at which point my brain was wired for the night and I struggled to get to sleep. I definitely wasn't taking the best care of myself over those 3 months.

Besides my self-care taking a nosedive, there were other mental and emotional challenges. The fear of failure sometimes made my brain want to skive off and play chess, and then I'd beat myself up about it.

> 🧠: You wanted to do this programming retreat and now here you are wasting your life playing chess! Maybe you're permanently broken and you'll never be productive ever again!

> Me: Why are you so mean?

In retrospect, I think I was still burnt-out from my PhD, which I'd just about wrapped up by the beginning of the batch.

The BitTorrent client dragged on for almost the entire 12 weeks, when I originally expected it to be a quick "warm-up" project. I could've picked up the same networking concepts by implementing a simpler protocol, and left more time for other topics. It also didn't lead to much interaction with other people because I was one of the few Recursers working on networking stuff. On the positive side, it did teach me lessons in persistence. There were moments where I despaired at the seemingly endless amount of work left to do, but by breaking it up into smaller tasks and chipping away at them every day, I eventually got to the finish line. I also figured out that it was important to let myself off the hook sometimes and work on Fun New Stuff. I could then return to the BitTorrent client feeling reinvigorated.

If I could redo my batch, I'd put more effort into scoping my projects, I'd allow myself to drop projects if they weren't serving my interests, and I'd join a study group (still feeling FOMO about the CryptoPals group). Also, I probably wouldn't read and write Zulip check-ins with such fanatical consistency. While it was helpful for feeling connected to my batchmates and led to some positive interactions, it took upwards of 30 minutes per day and I maybe could've spent that time talking to people directly.

I sometimes struggled with the interpersonal side of things. Zulip is a useful tool (when it's not blasting you with unwanted notifications), but it's also a reminder of why I shy away from social media. I found myself checking on my daily check-ins to see who had reacted to them (🐙!). Whenever there was a lack of engagement, or whenever I compared myself to other Recursers who seemed to have made stronger connections, it made me feel envious or down on myself. It can also be tough to see the many awesome things that other people are doing when you're having a bad day. (The double-edged sword of Learning Generously). This is all embarrassing to admit, but I think it's better to be open in case there are other people who have felt the same way. 

RC is, at the end of the day, a recruiting service. However, it's completely optional to engage with this side of things, which I appreciate. I had my CV reviewed and got some jobhunting tips, but if anything I should've made MORE use of their services. Despite this negligence on my part, the most life-impacting outcome of attending RC was that it netted me a job, quite by accident. I'd been planning a career pivot into data science and machine learning, and was building up a [portfolio](/dsml-portfolio.html) to that end, but hadn't gotten any interiews.

Then Amy (the only other Irish person attending RC during my batch) sent me a job posting that she thought would suit me. The job would involve designing and implementing programming languages. I immediately felt excited by this prospect - much more excited than by any of the data science jobs I'd seen. Funnily enough, here's something I said in my very first daily update:

> Feeling tempted by people talking about their programming language projects, makes me want to dive into the *Crafting Interpreters* book, or maybe *Engineering a Compiler*.

I never did end up reading *Crafting Interpreters* or *Engineering a Compiler*, but I'd made my own [tiny programming language before](https://github.com/Kevinpgalligan/ka), which is probably what got me the job. I'm now working on custom programming languages for a small Irish company called Manakau. I love working remotely (in my own timezone), my colleagues are really nice, and the programming is fun. Maybe I'll ask if I can write a blog post about it.

Overall, I loved my time at RC. The other day, I read over my daily updates for the first time since I'd finished the batch, and it brought back a lot of fond memories. I do think I've become a better programmer as a result of my time there, and being in that environment helped me to learn more about myself as a person, too. Not to mention that I got a job out of it.

On that note, it's time to close the book on my first ever batch at RC. I hope my batchmates are doing well, and I'd be delighted to hear from them if they read this!

`return Status.SUCCESS;`

[^return]: It took me an embarrassingly long time to realise that this is a [play on words](https://en.wikipedia.org/wiki/Return_statement).
