title: Get involved in open source software by using it
date: 2022-10-08
description: My uninformed thoughts on how to get involved in open source software.
publish: y
imgthumbnail: img/thumbnail.jpg
tags: rant

Beginner software developers often ask this question:

> Can someone tell me a good open source project to work on?

Even if someone suggests a project to work on, why would our hypothetical beginner, a burgeoning Linus Torvalds, care about a random project that someone pulled from their hat? It probably won't have any personal relevance to them. They probably won't know what it's for or how it works. In addition, big popular projects tend to have massive codebases with tens of thousands of lines of code, which could very well baffle a beginner.

Let me suggest instead that if you want to get involved in open source software, *use it*. My experience is that by using software, I have an idea of how it works. I already know something about the domain in which the software operates. The software is useful to me personally, so I'm motivated to fix it and improve it.

One type of open source software you might already use is the desktop application: Firefox, the GNOME calculator, tmux, various IRC clients, GIMP, vim. These are all applications that I use regularly, and if I encounter a bug in any of them, I'm motivated to fix it. For example: I [tried (and failed) to patch a bug](https://github.com/hexchat/hexchat/issues/2394#issuecomment-748543455) in the HexChat IRC application.

The problem with applications is that they generally have massive codebases. You won't necessarily be familiar with the programming language they use, and they require getting tangled up in application programming. You'll have to learn about GUI frameworks, how applications interact with operating systems, and other things that may or may not interest you.

But there's another way: use open source libraries in your own projects. You know, the same projects that you're doing to learn programming and to pad your CV. You will inevitably need to make use of code that other people have written. You will inevitably run into problems with that code, or identify a missing feature, at which point you can dig in and solve it yourself. The good thing is that you're motivated to solve the problem because it's your problem and it's blocking your project. Maybe you'll become particularly interested in one of these libraries and you can become actively involved in its development. And then all of a sudden you will have become an open source developer.

Another tip. The more niche the software you're using, the fewer eyes will be on the code and the more opportunity there will be to take things into your own hands. Using a popular Python package, there will be hundreds of people waiting to jump in and fix bugs. But for that Common Lisp package that only a dozen people use, you might be the only one who has experienced a particular bug, and you might be the only one who can fix it. For example, I contributed bugfixes to two Common Lisp libraries, [cl-sat](https://github.com/cl-model-languages/cl-sat/pull/5) and [random-state](https://github.com/Shinmera/random-state/pull/8), which I used in projects that I have described on this blog.

For experienced developers, let's stop recommending that new programmers launch themselves at large codebases and read them like a novel. [Nobody does that anyway](https://gigamonkeys.com/code-reading/). I still remember when I was learning Python through (and in spite of) the book Learning Python the Hard Way. The author recommended spending 1 week reading the codebase of a big web framework like Flask. 1 week was an annoyingly arbitrary timeframe, the objective of the task was not clear, and I didn't have a clue what a web framework was or what it was good for. My young self sensibly skipped the exercise. It's not like asking a journeyman painter to go to an art museum and learn from the work of the masters. We learn from the code of others by building a mental model of the problem in our head and then seeing the language idioms they used to solve it, not by aimlessly browsing their code with no idea what it's doing.