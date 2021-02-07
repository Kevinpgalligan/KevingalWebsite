title: 20 years without operator overloading in Java
date: 2021-01-15
description: 20 years have passed since Guy Steele's talk, "Growing a Language", and Java still doesn't have operator overloading.
requires: code math
tags: programming

A *syllable* is a sound that makes up part of a word. In the year two nought nought nought (2000), Guy Steele gave a talk called ["Growing a Language"](https://www.youtube.com/watch?v=_ahvzDzKdB0). He used a schtick in his talk: he did not use a word of more than one syllable if he did not first say what it meant. In this post, I use the same schtick.

As such, I will have to say what some words mean up-front, if you will bear with me.

A *language* is a set of words and rules for how to use them.

A *computer* is a tool that moves bits fast.

A *programming language* is a language that you use to tell a computer what to do.

*Java* is a programming language that is a cinch to run on any computer. It cleans up the junk left by your code, and takes care of a lot of small stuff like that for you.

Guy's talk was a talk on the Java programming language. He used the syllable schtick to show that if you take a small language, and you let folks add new rules and words to it, then they can turn it into a big, strong language. They can add what they need, and leave out what they don't need. They can grow the language in the way they want.

An *operator* is a blah.

*Operator overloading* is where a programming language lets you add rules, so that the way an operator acts is based on the stuff that you give it. You can add rules for the "plus" operator so that it works when you add counts (`1+1`), or strings (`"hello " + "world"`), or complex numbers (`Complex(1, 1) + Complex(1, 1)`). Java has some of these rules, but the folks who make Java can't think of all the rules you'll need in your life. That's why it's good to let folks add their own rules.

A *number*, by the way, is a count. A *complex number* is a number with a real and a non-real part.

To show a case where operator overloading is of great use, let's look at *BigInteger*, a type in the Java programming language that can store a number of any size. If you do math with a lot of BigIntegers, it's a mess. Take a look and see. What do you think this BigInteger math does?

    :::java
    public BigInteger f(BigInteger x) {
      return x.pow(2).multiply(2).plus(x.multiply(3)).minus(5);
    }

Here is the same code but with operator overloading.

    :::java
    public BigInteger f(BigInteger x) {
      return 2*x*x + 3*x - 5;
    }

It is the quadratic equation $`2x^2 + 3x - 5`$. A *quadratic equation* is... well, let's leave that for the next day. With no operator overloading, the code is hard to read and bugs can slip in. It gets worse as the number of BigIntegers you use goes up.

Some folks say that operator overloading is not worth it. Poor use of operator overloading is worse than any BigInteger mess, they say. But you can say that about any new thing that you might add to a programming language. And operator loading would be fab for those who want to use Java for math.

There are signs that [the folks who own Java](https://blogs.oracle.com/javamagazine/is-it-time-for-overloading-in-java) see the need for operating overloading. I sure hope that they, and the other Java folks, add operator overloading to Java, though I don't use Java so much these days.
