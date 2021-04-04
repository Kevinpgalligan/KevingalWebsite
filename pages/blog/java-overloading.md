title: Not Growing a Language
date: 2021-04-03
description: 20 years without operator overloading in Java.
requires: code
publish: y
tags: programming

A *syllable* is a sound that makes up part of a word. In the year two nought nought nought (2000), Guy Steele gave a talk called ["Growing a Language"](https://www.youtube.com/watch?v=_ahvzDzKdB0). He used a schtick in his talk, where he did not speak a word of more than one syllable if he did not first say what it meant.

In this post, I will use the same schtick. I have to say up front what some words mean, if you will bear with me.

A *language* is a set of words and rules for how to use them.

A *computer* is a tool that moves bits fast.

A *programming language* is a language that you use to tell a computer what to do.

*Java* is a programming language that is a cinch to run on any computer. It cleans up the junk left by your code, and takes care of a lot of small stuff like that for you.

Guy's talk was a talk on the Java programming language. He used the syllable schtick to show that if you take a small language, and you let folks add new rules and words to it, then they can grow the language in the way they want. They can add what they need, and leave out what they don't need. This way of thought may have come from Guy's work on the Scheme programming language.

An *operator* takes two things and maps them to a new thing. You will know the *addition* operator, which takes two numbers and adds them. A *number*, of course, is a count.

*Operator overloading* is where a programming language lets you add rules, so that the way an operator acts is based on the things that you give it. You can add rules for the addition operator so that it works when you add numbers (`1+1`), or strings (`"hello " + "world"`), or complex numbers (`Complex(1, 1) + Complex(1, 1)`). Java doesn't have operator overloading. It has *some*  rules for the addition operator, but the folks who make Java can't think of all the rules you'll need in your life. That's why it's good to let folks add their own rules.

A *complex number*, by the way, is a number with a real and a non-real part.

Guy Steele thought it would be neat if Java had operator overloading. In his talk he tried to show why it would be neat. At the time, he was part of the team that makes Java. As of now, he still seems to be part of that team. And yet, a score of years have passed, and here we are with no operator overloading. If Guy had a grave, he would turn in it, though to be sure of that we might have to ask him.

Let's look at *BigInteger*, a type in Java that can store a number of any size. If you do math with a lot of BigIntegers, it's a mess. Take a look and see. What do you think this BigInteger math does?

    :::java
    BigInteger f(BigInteger x) {
      return x.pow(2).multiply(2).plus(x.multiply(3)).minus(5);
    }

Here is the same code but with operator overloading.

    :::java
    BigInteger f(BigInteger x) {
      return 2*x*x + 3*x - 5;
    }

It's the quadratic equation 2x<sup>2</sup>+3x-5. A *quadratic equation* is... well, let's leave that for the next day. With no operator overloading, the code is hard to read and bugs can slip in. It gets worse as you use more and more BigIntegers.

Some folks say that operator overloading is not worth it. Poor use of operator overloading is worse than any BigInteger mess, they say. But with that line of thought you can nix *any* new tool that you might add to a programming language, so I don't think it's a strong case. And the fact is, operator overloading would be great for those who want to use Java for math.

There are signs that [the folks who own Java](https://blogs.oracle.com/javamagazine/is-it-time-for-overloading-in-java) see the need for operator overloading. I sure hope that they, and the rest of the Java team, add it to Java some day, though I haven't used Java in 2 years.

*Discussed on [Hacker News](https://news.ycombinator.com/item?id=26682582).*
