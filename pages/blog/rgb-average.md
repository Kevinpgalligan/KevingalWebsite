title: "Programming puzzle: random RGB tuple with average value of N"
date: 2019-10-14
draft: yes

Today we're going to examine a seemingly simple problem that will take us through the land of combinatorics. Something something describe the problem.

Possible problem statement: Generate a random RGB tuple where the average of R, G and B is N, N being an integer in the range [0, 255].

([Source](https://www.reddit.com/r/computerscience/comments/a1ea1s/create_program_to_generate_random_rgb_values_that/)).

We're given a target average value 'A' for the RGB tuple, so we need to satisfy the equation R + G + B = 3A.

TODO clean up this mess.

### First attempt
On first consideration of the problem, a simple algorithm comes to mind.

TODO make algorithm description less of a clusterfuck.

* Pick a random value for R so that R is in the range [0, 255] and 3A - R <= 2 * 255, otherwise G + B will be less than 3A - R and we won't be able to satisfy R + G + B = 3A.
* Pick a random value for G so that G is in the range [0, 255] and 3A - R - G <= 255.
* Set B = 3N - R - G.

Here it is in Python code.

    :::python
    import random as rd
    def generate_rgb(A):
        remaining = 3 * A
        R = rd.randint(max(remaining - 2 * 255, 0), min(remaining, 255))
        remaining -= R
        G = rd.randint(max(remaining - 255, 0), min(remaining, 255))
        remaining -= G
        B = remaining
        return R, G, B

This approach seems reasonable, and indeed, if we run a quick test, it passes with flying colours (no pun intended).

    :::python
    A = 1
    rgbs = [generate_rgb(A) for _ in range(10000)]
    print("PASS" if all(sum(rgb) // 3 == A for rgb in rgbs) else "FAIL")
    # output: PASS

However, this doesn't give us the full picture. Besides ensuring that R + G + B = 3A, we also need to validate that R, G and B have the same distribution of values as each other. R should be equally as likely as G and B, for example, to have a value of 2. Graphing the distribution of values shows that R is actually more likely to have high values than G and B.

![distribution of values for R, G, B]({{ url_for('static', filename='img/rgb-average/value-frequency-attempt1.png') }})

This is because there are 4 possible values for R, and the probability that R takes on any particular value, with our algorithm, is 1/4. For example, P(R=3) (the probability of R=3) is 1/4, while P(G=3) = P(B=3) = P(R=0) \* 1/4 = 1/16.

TODO more explanation for last paragraph.

TODO one aspect of our algorithm was correct, another was not.

### What's the fix?
The problem is that we can't assign R a value from its range of possible values with equal probability, because some of the values are more likely than others depending on the values of G and B. For example, in the case where N = 1, P(R=3) < P(R=1) because R=3 is only possible in the case where (G,B)=(0,0), whereas R=1 is possible for (G,B) eof {(0,2), (2,0), (1,1)}.

So, we have to form a probability distribution for the value of R. We have P(R=k) = P(G+B=3N-k) = (number of combinations of G & B that equal 3N-k) / (total number of possible combinations of G & B) = (3N-k+1) / (3N+1) = 1 - k/(3N+1). WRONGGGGG.

After we have the value of R, then G and B have an equal chance of assuming any of the remaining values.

Here it is in Python code.

TODO

A probability distribution of the possible values of R for various N.

TODO

And finally, our test of the value frequency of R, G and B for N=1.

TODO

Bonus test: frequency of all of the possible RGB tuples for N=5. They should all be similar.

TODO

### Generalisation
**TODO**

Generate tuple V = (V1, V2, ..., Vn) such that average(V) = K, where Bl <= Vi, K <= Bu.

Should be easy nuff.

### Is there a way to do it without the shuffle?
**TODO**

