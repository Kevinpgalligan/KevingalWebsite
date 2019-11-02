title: A random RGB tuple whose average is N
date: 2019-10-14
draft: yes

I recently heard a story about a master computer scientist and their student. It went something like...

> Master: Generate a random RGB tuple where the average of R, G and B is N, N being an integer in the range [0, 255].

> Student: Why? What practical application does this have?

> Master: Yes.

([Source](https://www.reddit.com/r/computerscience/comments/a1ea1s/create_program_to_generate_random_rgb_values_that/)).

Today, we're going to be that student.

### First attempt
On first consideration of the problem, a simple algorithm comes to mind.

* Pick a random value for R so that (R + G + B) / 3 = N is still possible.
* Pick a random value for G so that (R + G + B) / 3 = N is still possible.
* Set B = 3N - R - G.

Here it is in Python code. **TODO improve explanation of the algorithm.**

    :::python
    import random as rd
    def generate_rgb(N):
        remaining = 3 * N
        # We can't give R a value outside this range or it becomes
        # impossible to achieve an average of N. If we pick an R
        # less than 'remaining - 2 * 255', then we will always have
        # that R + G + B < 3N, even if both G and B have values of 255.
        R = rd.randint(max(remaining - 2 * 255, 0), min(remaining, 255))
        remaining -= R
        G = rd.randint(max(remaining - 1 * 255, 0), min(remaining, 255))
        remaining -= G
        B = remaining
        return R, G, B

This approach seems reasonable, and indeed, if we run a quick test, it passes with flying colours (no pun intended).

    :::python
    N = 1
    rgbs = [generate_rgb(N) for _ in range(10000)]
    print("PASS" if all(sum(rgb) // 3 == N for rgb in rgbs) else "FAIL")
    # output: PASS

However, graphing the value frequency shows that our algorithm R is more likely to have high values than G and B.

![frequency of values for R, G, B]({{ url_for('static', filename='img/rgb-average/value-frequency-attempt1.png') }})

**TODO improve this explanation.**

This is because P(R=3) (the probability of R having a value of 3), for example, is 1/4, while P(G=3) and P(B=3) are both P(R=0)\*(1/4) = 1/16, since it's impossible for G or B to be 3 unless R is 0.

### What's the fix?
All we have to do is shuffle the values before returning from our function, so change this:

    :::python
    def generate_rgb(N):
        # ...
        return R, G, B

To this:

    :::python
    def generate_rgb(N):
        # ...
        result = [R, G, B]
        rd.shuffle(result)
        return tuple(result)

And here's our new distribution of values, which appears to be balanced.

![frequency of values for R, G, B]({{ url_for('static', filename='img/rgb-average/value-frequency-attempt2.png') }})

### Is there a way to do it without the shuffle?
**TODO**

### Generalisation
**TODO**

Generate tuple V = (V1, V2, ..., Vn) such that average(V) = K, where Bl <= Vi, K <= Bu.

Should be easy nuff.
