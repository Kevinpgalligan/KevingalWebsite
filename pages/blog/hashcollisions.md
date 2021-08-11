title: The probability of a hash collision
date: 2020-08-09
description: Exploring the math behind hash collisions.
imgthumbnail: img/hashcollisions/thumbnail.jpg
requires: math
tags: probability

A [hash collision](https://en.wikipedia.org/wiki/Hash_collision) happens when two items turn out to have the same hash value. You take the first item, compute its hash value, and put it in the corresponding hash bucket. Then you take the second item, compute its hash value, and - woops, the first item is already in that bucket! This is bad because it can make your hash table run more slowly. It is also a problem if you intend to use an item's hash value as its unique identifier. Obviously, the identifier will no longer be unique if it's shared by two or more items!

So, given $`N`$ hash buckets and $`k`$ items, what's the probability of a hash collision? Jeff Preshing wrote a [neat article](https://preshing.com/20110504/hash-collision-probabilities/) on how to calculate hash collision probabilities, but there are some gaps in his explanation. This is my attempt to fill those gaps. To follow along you'll need a basic understanding of probability and an iron will.

### The Birthday Problem
Consider the following problems.

1. When you put 50 balls into 100 buckets at random, what's the probability that at least 2 balls will end up in the same bucket? (That would be 99.999969%).

2. When there are 30 people in a room, what's the probability that at least 2 of them will share a birthday? (I'll take 70.6316% for 10, Alex).

3. And when you compute the 32-bit hash value of 5000 different strings, what's the probability that at least 2 strings will have the same hash value? (TODO).

These are all versions of the [birthday problem](http://en.wikipedia.org/wiki/Birthday_problem), which is notorious for having an unintuitive answer. The key to understanding why the probability is so high is that, even though there might only be 50 balls in problem #1, there are 1275 *pairs* of balls, and only one of those pairs needs to be in the same bucket. Problems #2 and #3 have the exact same structure, because in problem #2, you can think of the people as balls and the days of the year as buckets. And in problem #3, you can think of the strings as balls and the possible hash values as buckets.

The birthday problem IS the problem of calculating hash collision probability. Let's take a look at how to calculate it.

### Exact calculation of birthday probability
To calculate the exact probability of two or more people sharing a birthday in a room of $`k`$ people, you have to consider the probability of each person *not* sharing a birthday with anyone else who is already in the room.

The first person to enter the room has a probability of

```math
\frac{N}{N} = 1
```

of not sharing a birthday with anyone, because there's nobody else there!

The probability that neither the first person nor the second person to enter the room will share a birthday is

```math
\frac{N}{N} \times \frac{N-1}{N},
```

because there are $`N-1`$ out of $`N`$ days free for the second person.

### Exponent approach
Show how to go from the above to e^(-k(k-1)/2N), using 1-x=e^-x for small x.

Then hash collision probability is 1-e^(...)

### Simple approximation
We can use the exact same trick again; our hash collision probability is p=1-e^-y. We already know from above that (approximately) 1-y=e^-y when y is small; therefore we can rearrange to get p=1-e^-y=y.

Plot ideas: 1-x versus full e^-x plot; plot all the different approximations (or some measure of how close they are).

--------------

<img src="{{ url_for('static', filename='img/basketball/around-the-world.png') }}"
     alt="A variation of Around the World. 7 shooting positions, evenly spaced in a semi-circle around the hoop."
     class="centered">

[^infinite]: As you might have guessed, we don't compute the entire infinite sum to generate this graph. We stop after enough terms have been added that the error in the result is small.
