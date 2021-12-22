title: The probability of a hash collision
date: 2021-12-21
description: Exploring the math behind hash collisions.
imgthumbnail: img/hashcollisions/thumbnail.jpg
requires: math
tags: probability

A [hash function](https://en.wikipedia.org/wiki/Hash_function) takes arbitrarily complex input - a word, a website, an image, a human being - and maps it to a single number. This is useful for various computer science stuffs, such as data storage.

For example, let's say you want to store a book in one of $`N`$ boxes. If you put the book in a random box, it's quite likely that you'll forget which box you picked, especially as $`N`$ becomes large. What you can do instead is apply a hash function to the title of the book (probably *The Notebook*), which will spit out a number, $`i`$. You then put your book in the $`i`$-th box[^modulo]. When you want to retrieve *The Notebook* again, you recompute the hash function and it tells you to look in the $`i`$-th box, without you needing to remember anything! You won't lose a book from your Nicholas Sparks collection ever again.

We won't get into the specifics of what a hash function looks like. This post is instead concerned with [hash collisions](https://en.wikipedia.org/wiki/Hash_collision), which happen when your hash function assigns the same value to two different items. This might be a Bad Thing. Our hash function wouldn't be very useful, for instance, if it told us to put all our books in the 1st box, because we'd have to spend a lot of time rooting around in that box to find a specific book[^unique]. For this reason, a good hash function should have evenly distributed outputs. But even then, it's only a matter of time before a hash collision occurs as we add more and more books to our collection, and by the time we reach $`N+1`$ books, there won't enough boxes to store the books individually and there will definitely be at least 1 hash collision.

Given $`N`$ boxes and $`k`$ books, how do you figure out the *probability* of a hash collision? Hash collisions can be a Bad Thing, but rather than trying to eliminate them entirely (an impossible task), you might instead buy enough boxes that the probability of a hash collision is relatively low.

Jeff Preshing wrote a [neat article](https://preshing.com/20110504/hash-collision-probabilities/) on how to calculate hash collision probabilities, but there are some gaps in his explanation. This is my attempt to fill those gaps. To follow along you'll need a basic understanding of probability and an iron will.

### The Birthday Problem
Consider the following problem statements.

1. When you compute the hash values of 500 different book titles from your magnificent book collection, and put each book into the corresponding box in a row of 100,000 boxes (yes, it's a lot of boxes), what's the probability that at least 2 books will be in the same box? (TODO).

2. When you put 50 balls into 100 buckets at random, what's the probability that at least 2 balls will end up in the same bucket? (99.999969%).

3. When there are 30 people in a room, what's the probability that at least 2 of them will share a birthday? (70.6316%).

These are all versions of the [birthday problem](http://en.wikipedia.org/wiki/Birthday_problem), which is notorious for having an unintuitive answer. The key to understanding why the probabilities are so high is that, even though there might be only 50 balls in problem #2, there are 1275 *pairs* of balls, and only one of those pairs needs to be in the same bucket. Problems #1 and #3 have the exact same structure. In problem #1, you can think of the books as balls and the boxes as buckets, and in problem #3, the people are balls and the days of the year are buckets.

The problem of calculating hash collision probabilities IS the birthday problem. Let's look at how to solve the birthday problem.

### Exact calculation of birthday probability
To calculate the exact probability of two or more people sharing a birthday in a room of $`k`$ people, it's easiest to first figure out the probability of *none of them* sharing a birthday. In this case, $`N=365`$, since instead of boxes there are 365 possible birthdays.

The first person to enter the room has probability

```math
\frac{N}{N} = 1
```

of not sharing a birthday with anyone, because duh, there's nobody else in the room!

The probability that the first and second person to enter the room won't share a birthday is

```math
\frac{N}{N} \times \frac{N-1}{N},
```

because there are $`N-1`$ out of $`N`$ days free for the second person.

This continues until everyone is in the room and we get that the probability of no shared birthday is

```math
\frac{N}{N} \times \frac{N-1}{N} \times ... \times \frac{N-(k-1)}{N}.
```

The probability of at least 1 shared birthday, i.e. of a hash collision, is then the complement of there being no shared birthdays:

```math
1 - \frac{N}{N} \times \frac{N-1}{N} \times ... \times \frac{N-(k-1)}{N}.\tag{1}
```

And... that's it. That's the EXACT probability of a hash collision for $`k`$ items and $`N`$ possible hash function outputs! All right, everyone, pack your bags and go home. There's nothing more to see here.

But actually, this formula gets super awkward as $`N`$ and $`k`$ get big. Imagine you have 1 billion books. Do you really want to multiply 1 billion numbers together? I bet not. Even a computer will take a long time to multiply 1 billion numbers. For this reason, approximations of the formula are often used, as we will see next.

### e-asy approximation
A first approximation of the hash collision probability can be derived by rewriting equation (1) so that the terms in the product are written as complements:

```math
1 - \left(1 - 0\right)\left(1 - \frac{1}{N}\right)...\left(1-\frac{k-1}{N}\right)
```

Now, here comes a trick. It's a fact that $`1-x \approx e^{-x}`$ as $`x`$ gets small (see Appendix A for a proof, which was omitted from Preshing's article). You might notice that we rewrote the terms in the product to match the form $`1-x`$, where $`x=i/N`$ for varying $`i`$. Thus, we can use that neat little approximation to get

```math
\begin{aligned}
&1 - \left(1 - 0\right)\left(1 - \frac{1}{N}\right)...\left(1-\frac{k-1}{N}\right) \\
&=1-e^0 \times e^{-1/N}\times ... \times e^{-(k-1)/N} \\
&= 1 - e^{-\frac{1+2+...+(k-1)}{N}} \\
&= 1 - e^{-\frac{k(k-1)}{2N}}. \tag{2}
\end{aligned}
```

The last equality comes from the fact that $`1+2+...+n=n(n+1)/2`$.

This approximation reduces the complexity of computing the probability from $`\mathcal{O}(k)`$ to $`\mathcal{O}(1)`$. Or, phrased in non-computer-science terms: instead of doing around $`k`$ math operations, we only have to do a few of them! What's more, the approximation becomes more and more accurate as we increase $`N`$.

### Approximations all the way down
It's possible to simplify this equation even further, using the exact same trick as before! Note that the final line of equation (2) is in the form $`1-e^{-y}`$, where $`y=\frac{k(k-1)}{2N}`$. We've already seen that $`1-e^{-y} \approx y`$ when $`y`$ is small, and so the hash collision probability can be further approximated by:

```math
\frac{k(k-1)}{2N}. \tag{3}
```

The final approximation suggested by Preshing is that $`k(k-1)`$ starts to look an awful lot like $`k^2`$ as $`k`$ gets bigger (see Appendix B), so it can be written simply as

```math
\frac{k^2}{2N}. \tag{4}
```

And there you have it. 4 different methods of calculating the hash collision probability, with varying levels of accuracy.

### Comparisons
How do these methods compare? Here are some plots!

### Conclusions
We have seen how to calculate the probability of a hash collision, as well as 3 different ways to approximate it. I've implemented all of this as a [web app](linkhere), which you can play around with for your edutainment. May your books be evenly distributed in your boxes.

TODOs summary: proofs, web app, plots, calculate probability of the birthday problem example

### Appendix A: Supporting proof for e-based approximation 
TODO

### Appendix B: Supporting proof for k(k-1)=k(k)
TODO

[^modulo]: If $`N<i`$, then you take $`i+1 \pmod{N}`$. In other words, when you're counting up to the $`i`$-th box, loop back to the 1st one if you ever exceed $`N`$.

[^unique]: Here's another motivation for avoiding hash collisions. Let's say you're designing a customer database for a particularly privacy-conscious fish monger. Rather than storing a customer's name in the database, which would be a horrific breach of privacy, you can instead apply a hash function to their name and store the resulting hash value. A hash collision occurs when two customers are assigned the same hash value by the hash function, which would result in fish orders of the customers getting mixed up. How embarrassing! What this means is that you need the range of the hash function (the number of buckets, $`N`$), to be large enough that a hash collision is ASTRONOMICALLY UNLIKELY.
