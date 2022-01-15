title: The probability of a hash collision
date: 2022-01-15
description: Exploring the math behind hash collisions.
imgthumbnail: img/collisions/thumbnail.jpg
requires: math
publish: y
tags: probability

A [hash function](https://en.wikipedia.org/wiki/Hash_function) takes arbitrarily complex input - a word, a website, an image, a human being - and maps it to a single number. This is useful for various computer science stuffs, such as data storage and cryptography.

For example, let's say you want to store a book in one of $`N`$ boxes. If you put the book in a random box, it's quite likely that you'll forget which box you picked, especially as $`N`$ gets bigger. What you can do instead is apply a hash function to the title of the book (probably *The Notebook*, knowing you), which will spit out a number, $`i`$. You then put your book in the $`i`$-th box[^modulo]. When you want to retrieve *The Notebook* again, you recompute the hash function and it tells you to look in the $`i`$-th box, without you needing to remember where you stored anything! You won't lose a book from your Nicholas Sparks collection ever again.

We won't get into the specifics of what a hash function looks like. This post is instead concerned with [hash collisions](https://en.wikipedia.org/wiki/Hash_collision), which happen when your hash function assigns the same value to two different items. This might be a Bad Thing. Our hash function wouldn't be very useful, for instance, if it told us to put all our books in the 1st box, because we'd have to spend a lot of time rooting around in that box to find a specific book[^unique]. For this reason, a good hash function should have evenly distributed outputs. But even then, it's only a matter of time before a hash collision occurs as we add more and more books to our collection. By the time we reach $`N+1`$ books, there won't enough boxes to store the books individually and there will definitely be at least 1 hash collision.

Given $`N`$ boxes and $`k`$ books, how do you figure out the *probability* of a hash collision? Hash collisions can be a Bad Thing, but rather than trying to eliminate them entirely (an impossible task), you might instead buy enough boxes that the probability of a hash collision is relatively low.

Jeff Preshing wrote a [neat article](https://preshing.com/20110504/hash-collision-probabilities/) on how to calculate hash collision probabilities, but there are some gaps in his explanation. This is my attempt to fill those gaps. To follow along you'll need a basic understanding of probability and an iron will.

### The Birthday Problem
Consider the following problem statements.

1. When you compute the hash values of 500 different book titles from your magnificent book collection, and put each book into the corresponding box in a row of 100,000 boxes (yes, it's a lot of boxes), what's the probability that at least 2 books will be in the same box? (~71.3%).

2. When you put 50 balls into 100 buckets at random, what's the probability that at least 2 balls will end up in the same bucket? (~99.99997%).

3. When there are 30 people in a room, what's the probability that at least 2 of them will share a birthday? (~70.6%).

These are all versions of the notorious [birthday problem](http://en.wikipedia.org/wiki/Birthday_problem). It's notorious because it's not immediately intuitive why the probabilities are so high. The key is that even though there are only 50 balls in problem #2, there are 1225 *pairs* of balls, and only one of those pairs needs to be in the same bucket. Problems #1 and #3 have the exact same structure. In problem #1, you can think of the books as balls and the boxes as buckets, and in problem #3, the people are balls and the days of the year are buckets.

The problem of calculating hash collision probabilities IS the birthday problem. Let's look at how to solve the birthday problem.

### Exact calculation of birthday probability
To calculate the exact probability of two or more people sharing a birthday in a room of $`k`$ people, it's easiest to first figure out the probability of *none of them* sharing a birthday. In this case, $`N=365`$, since instead of boxes there are 365 possible birthdays.

The first person to enter the room has probability

```math
\frac{N}{N} = 1
```

of not sharing a birthday with anyone, because duh, there's nobody else in the room!

The probability that the first and second person to enter the room don't share a birthday is

```math
\frac{N}{N} \times \frac{N-1}{N},
```

because there are $`N-1`$ out of $`N`$ days free for the second person.

The probability that the first, second and third person don't share a birthday is

```math
\frac{N}{N} \times \frac{N-1}{N} \times \frac{N-2}{N},
```

This continues until everyone is in the room and we get that the probability of no shared birthday is

```math
\frac{N}{N} \times \frac{N-1}{N} \times ... \times \frac{N-(k-1)}{N}.
```

The probability of at least 1 shared birthday, i.e. of a hash collision, is then the complement of there being no shared birthdays:

```math
1 - \frac{N}{N} \times \frac{N-1}{N} \times ... \times \frac{N-(k-1)}{N}.\tag{1}
```

And... that's it. That's the EXACT probability of a hash collision for $`k`$ items and $`N`$ possible hash function outputs! All right, pack your bags and go home. Nothing more to see here.

But actually, this formula gets clunky as $`N`$ and $`k`$ get big. Imagine you have 1 billion books. Do you really want to multiply 1 billion numbers together? I bet not. Even a computer will take a long time to multiply 1 billion numbers. For this reason, approximations of the formula are often used, as we will see next.

### e-asy approximation
A first approximation of the hash collision probability can be derived by rewriting equation (1) so that the terms in the product are written as complements:

```math
1 - \left(1 - 0\right)\left(1 - \frac{1}{N}\right)...\left(1-\frac{k-1}{N}\right)
```

Now, here comes a trick. It's a fact that $`1-x \approx e^{-x}`$ as $`x`$ gets small (see Appendix A for a proof, which was omitted from Preshing's article). You might notice that we rewrote the terms in the product to match the form $`1-x`$, where $`x=i/N`$ for varying $`i`$. Since $`N`$ is presumably much larger than $`i`$ and $`x`$ is therefore small, we can use our cute little approximation to get

```math
\begin{aligned}
&1 - \left(1 - 0\right)\left(1 - \frac{1}{N}\right)...\left(1-\frac{k-1}{N}\right) \\
&=1-e^0 \times e^{-1/N}\times ... \times e^{-(k-1)/N} \\
&= 1 - e^{-\frac{1+2+...+(k-1)}{N}} \\
&= 1 - e^{-\frac{k(k-1)}{2N}}. \tag{2}
\end{aligned}
```

The last equality comes from the fact that $`1+2+...+n=n(n+1)/2`$.

This approximation reduces the complexity of computing the probability, from $`\mathcal{O}(k)`$ to $`\mathcal{O}(1)`$. Or, phrased in non-computer-science terms: instead of doing around $`k`$ math operations, we only have to do a small fixed number of them! What's more, the approximation becomes more and more accurate as we increase $`N`$.

### Approximations all the way down
It's possible to simplify this equation even further, using the exact same trick as before! Note that the final line of equation (2) is in the form $`1-e^{-y}`$, where $`y=\frac{k(k-1)}{2N}`$. We've already seen that $`1-e^{-y} \approx y`$ when $`y`$ is small, and so the hash collision probability can be further approximated by:

```math
\frac{k(k-1)}{2N}. \tag{3}
```

...which is funny, because $`\frac{k(k-1)}{2}`$ is the number of pairs of items, which I brought up earlier to provide an intuition for the birthday problem!

The final approximation suggested by Preshing is that $`k(k-1)`$ starts to look an awful lot like $`k^2`$ as $`k`$ gets bigger (see Appendix A), so the formula can be written simply as

```math
\frac{k^2}{2N}. \tag{4}
```

And there you have it. 4 different methods of calculating the hash collision probability, with varying levels of accuracy.

### Comparisons
How do the approximations fare? Here's a plot of their relative error compared to the exact birthday calculation, for $`N=10000`$.

<figure>
<img src="{{ url_for('static', filename='img/collisions/error.png') }}"
     alt="Relative error of the approximations. k goes from 2 to 10000 (ish)."
     class="centered">
<figcaption>Relative errors of the three approximation methods: the exponential approximation (exp), the simplified version (simple), and the even more simplified version (simple-square).</figcaption>
</figure>

The relative error between two quantities $`x`$ and $`y`$ is $`err(x,y)=\frac{\lvert x - y \rvert}{x}`$, and the smaller it is, the better. If $`err(x,y)<1`$, the approximation is within an order of magnitude of the true value. We see that the exponential approximation gradually gets better as $`k`$ gets smaller. But also, once $`k>300`$ or thereabouts, the relative error drops like a stone. The simpler approximations are kinda reasonable for $`k<100`$, but after that point they go bonkers.

HERE'S A SAMPLE DATAPOINT. At $`k=10`$, the real collision probability is about $`0.0044913`$. The exponential approximation is about $`0.0044899`$, and if you squint a little, you could mistake them for the same answer. The simplified approximation is about $`0.0045`$, which is, you know, still quite good. And the simplest approximation is $`0.005`$, which is less quite good. 

I was going to dig into the error behaviour of the approximations, but I think I'll get a sandwich instead. Something something exercise for the reader.

### Conclusions
We have seen how to calculate the probability of a hash collision, as well as 3 different ways to approximate this probability. The exponential approximation appears to be robust. The other two are convenient for back of the envelope calculations, but may lose their nerve as you add more books to your collection. Finally, if you want to play around with the birthday problem parameters, I've posted a [hash collision calculator](/apps/collision.html) on the site.

### Appendix A: Supporting proofs for approximations 
How do we prove that the approximations we've used are "good" in some sense? Or more specifically, that:

1. $`k(k-1) \sim k^2`$ as $`k \to \infty`$.
2. $`1-x \sim e^{-x}`$ as $`x \to 0`$.

This might seem obvious (in the case of #2, just set x=0 and they're the same!), but I was wondering how you would go about it with slightly more rigour.

One approach is based on ratios. Use $`f(x)`$ to denote the function being approximated, and $`g(x)`$ to denote the approximation, and show that $`\lim_{x \to a} \frac{f(x)}{g(x)}=1`$. Since $`\frac{u}{v}=1`$ implies $`u=v`$, this means that $`f(x)`$ and $`g(x)`$ converge in some sense.

I've had a shot at the proofs, below. I'm not a mathematician, so don't take them seriously, although the sages of r/learnmath did [review them for me](https://www.reddit.com/r/learnmath/comments/s3cf8s/analysis_proving_that_these_are_good/).

For #1, the proof is:

```math
\begin{aligned}
\frac{k^2}{k(k-1)} = \frac{k^2}{k^2-k} = \frac{k^2}{k^2} \times \frac{1}{1-1/k} = \frac{1}{1-1/k} \mathrel{\underset{k \to \infty}{=}} \frac{1}{1-0} = 1.
\end{aligned}
```

For #2, we use the Taylor series expansion of $`e^{-x}`$,

```math
e^{-x} = 1 - \frac{x}{1!} + \frac{x^2}{2!} - ...
```

Applying this expansion to the ratio, we get

```math
\begin{aligned}
\frac{1-x}{e^{-x}} &= \frac{1-x}{1-\frac{x}{1!}+\frac{x^2}{2!} - ...} \mathrel{\underset{x \to 0}{=}} \frac{1 - 0}{1 - 0 + 0 - ...} = 1.
\end{aligned}
```

[^modulo]: If $`N<i`$, then you take the book at position $`i+1 \pmod{N}`$. In other words, when you're counting up to the $`i`$-th box, loop back to the 1st one if you ever exceed $`N`$.

[^unique]: Here's another motivation for avoiding hash collisions. Let's say you're designing a customer database for a particularly privacy-conscious fish monger. Rather than storing a customer's name in the database, which would be a horrific breach of privacy, you can instead apply a hash function to their name and store the resulting hash value. A hash collision occurs when two customers are assigned the same hash value by the hash function, which could result in the customers' fish orders getting mixed up. How embarrassing! What this means is that you need the range of the hash function (the number of buckets, $`N`$), to be large enough that a hash collision is ASTRONOMICALLY UNLIKELY.
