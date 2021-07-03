title: Approaching things backwards
date: 2021-07-02
description: On getting involved in open source software.
imgthumbnail: img/easy/thumbnail.jpg
publish: y
tags: rant

Here's a common question that fresh software developers have.

> Can you suggest a good open source project to learn from and potentially contribute to?

This reminds me of an experience I had in my early programming days. I was working through an online course called *Learn Python the Hard Way*[^hardway], when I was instructed to find an open source project and just... read the source code. For a whole week. I took this in good faith and visited some of the suggested projects, which, it was claimed, were the pinnacles of Python programming style. But I had no idea what they were for. "What's this Flask thing? What's a web server?". I rooted through the source tree of some of these projects (which, by the way, is confusing to navigate for a beginner) and uncovered some Python source files. I maybe dug around in the source code for a while, but this was perfectly confusing. "A BarnacleConnector class? What's that?" I was completely lost and became disillusioned.

There's this idea that you can blah blah blah. The thing is, this is an ass-backwards approach to getting involved in open source software. How are you supposed to take a random open source project, with 10s of thousands or 100s of thousands or millions of lines of code, and even begin to understand how it works and what it's useful for? How are you supposed to motivate yourself to give a shit about it?

I would argue that you can't. Or at least, that it's much more difficult than it needs to be. Over the years, I have discovered that it's much easier to approach this problem from the other direction. Rather than setting out in an arbitrary direction and picking a random software project that someone suggests to you, the first step to getting involved in open source software should be to *use it*. Is there a cool personal project you want to work on? Well, as your projects grow in complexity, they'll require more open source packages. And those packages will, at some point, break. At which point, someone will have to step in and fix them -- and that person can be you! And believe me, open source packages do break.

Even if a package doesn't break and you never get your break-out opportunity, by using the package (1) it's useful for you, (2) you understand what it's used for, (3) you might have an idea of how it works, at least from an external perspective, (4) you might be curious about how it works internally, (5) maybe there's a feature that would be super useful for you that hasn't been added yet. That's like 5 things that could motivate you or help you to get involved!

You don't even have to use it as part of your own projects. Just use cool software day-to-day, like a calendar or a calculator or what have you. And maybe you'll find bugs in this software, or you'll find yourself wishing for a feature that's missing. Then you have the perfect opportunity to step in and make things better (assuming you use open source software; if not, then it's time to change that).

I'm not an expert on the "philosophy" of open source software, but this seems like a more sustainable dynamic for the open source ecosystem in general. Software that is maintained by people who care about it and who are motivated to develop it and who use it every day. As opposed to random people coming along, who don't care about the software or have any particular use for it, who contribute the minimum required amount to pad their CV and then move on. That's an unsustainable approach, you can't expect people to work on software that they have no reason to give a shit about. And you can't expect yourself to be motivated to dig into software, or to want to contribute to it, if it doesn't mean anything to you.

SOMETHING ABOUT READING. Even experienced people don't read source code for fun (cite gigamonkey). Not without motivation. It's better to read source code when you have a pressing need. You're calling a function and it's not working how you expect. So you go to the source code to figure it out, and oh!- isn't it cool how on this line they use this built-in function in this cool way? That's a good trick, I'll remember that one for next time. Now where was I, how does this function work...

[^hardway]: Which is garbage, by the way. As the name implies, there are much easier and more effective ways to learn Python.
