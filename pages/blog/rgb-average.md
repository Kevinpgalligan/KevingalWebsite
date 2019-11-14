title: "Random RGB values that average to N: adventures in probability and combinatorics"
date: 2019-11-10
draft: yes

On this episode of Problems That Don't Need to Be Solved, we're going to take on the [following challenge](https://www.reddit.com/r/computerscience/comments/a1ea1s/create_program_to_generate_random_rgb_values_that/):

> Generate a random RGB value where the average of R, G and B is A, A being an integer in the range [0, 255].

In other words, we have to find a random RGB value so that (R + G + B) / 3 = A; multiplying both sides of the equation by 3, we can also write it as  R + G + B = 3A.

Solving this seemingly simple problem, we'll encounter the difficulty of testing probablistic algorithms, some surprisingly tricky combinatorics, and a few graphs of red, green and blue. TODO refine this line

### An attempt
The first algorithm that might come to mind is the following:

* Pick a random value for R so that R is in the range [0, 255], R <= 3A, and 3A <= R + 255 + 255. The last requirement is non-obvious, but if we don't satisfy it, then even with G = B = 255, R + G + B will still be less than 3A. It can also be written as 3A - 2 * 255 <= R.
* Pick a random value for G so that G is in the range [0, 255], G <= 3A - R, and 3A - R - 255 <= G (same reason as before).
* Set B = 3N - R - G.

Here it is in Python code. Hopefully the equivalence between the description above and the code is clear.

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

This approach seems reasonable, and if we run a test that checks whether the resulting RGB tuples have an average of A, it passes with flying colours (pun unintended).

    :::python
    A = 1
    rgbs = [generate_rgb(A) for _ in range(10000)]
    print("PASS" if all(sum(rgb) // 3 == A for rgb in rgbs) else "FAIL")
    # output: PASS

However, this doesn't give us the full picture. Besides ensuring that R + G + B = 3A, we also need to validate that R, G and B have the same distribution of values as each other. R should be equally as likely as G and B, for example, to have a value of 2. Graphing the distribution of values shows that R actually tends to have different values to G and B. TODO rewrite last sentence, it's confusing

![distribution of values for R, G, B]({{ url_for('static', filename='img/rgb-average/value-distribution-broken.png') }})

This is because, for A=1, R has a 1/4 chance of taking on any of its possible values (which are: 0, 1, 2, 3). However, P(G=3) -- the probability that G=3 -- is P(R=0) \* P(G=3|R=0) = 1/4 * 1/4 = 1/16. We require that R=0 so that 3 remains one of the possible values for G, and that 3 is then pulled out of the hat of possible values of G with a probability of 1/4.

We see that, although our simple algorithm is correct in the sense that it generates RGB values with the correct average, it doesn't generate all of the possible RGB values with equal probability, which is one of the requirements of the problem. We need the RGB values to be truly random.


### What's the right approach?
The problem is that we can't assign R a value from its range of possible values with equal probability, because some of the values are more likely than others, depending on the values of G and B. For example, in the case where A=1, P(R=3) < P(R=1) because R=3 is only possible in the case where (G,B)=(0,0), whereas R=1 is possible when (G,B) is any of (0,2), (2,0) and (1,1).

So, we have to form a probability distribution for the value of R. The probability that R=k for some k is given by #{combinations of G & B that equal 3A-k} / #{RGB values that average to A}.

To calculate this, we will need to delve into some tricky combinatorics. Let's take the numerator and the denominator of the expression separately.

Here's the formula for #{combinations of G & B that equal 3A-k}:

![numerator formula, min(3A-k, 255) - max(3A-k-255, 0) + 1]({{ url_for('static', filename='img/rgb-average/equation-1-numerator.png') }})

It's easiest to understand this with an example. If 3A-k=258, then the maximum value that G can take is 255, which is given by min(3A-k, 255) = min(258, 255) = 255. The minimum value it can take, while still possibly satisfying G+B=3A-k, is 3. This is given by max(3A-k-255, 0) = max(258-255, 0) = max(3, 0) = 3. So we subtract the maximum possible value of G from the minimum, and add 1 since it's an inclusive range, to get the number of possible combinations of GB.

Now we need to calculate the denominator, #{RGB values that average to A}, in our expression to give the probability of R=k. It looks scary, but we'll explain each part in an intuitive manner.

![denominator formula, binom(3A+3-1, 2) - 3 * binom(3A+3-1-256, 2) + 3 * binom(3A+3-1-2\*256, 2)]({{ url_for('static', filename='img/rgb-average/equation-2-denominator.png') }})

The first part, **in black**, is the number of ways to divide 3A between R, G and B.

To understand this, let's consider an example where A=2, 3A=6. Imagine that we have the numbers 1-6 lined up with slots between them. We have to put "partitions" in 2 of the slots in order to divide 3A between R, G and B, as seen below. (By the way, this approach to explaining combinatorics is called [stars and bars](https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics))).

![visualisation of partitioning of 3A]({{ url_for('static', filename='img/rgb-average/comb-explained-1.png') }})

We have 3A-1 = 5 slots, and we have to insert partitions into 2 of them, which gives 5C2 possible combinations. HOWEVER, we also have to account for the case where R, G or B are 0. For that reason, we need 3 extra slots that let us set each of R, G and B to 0.

Here we see the case where G=0.

![visualisation of partitioning of 3A, part 2]({{ url_for('static', filename='img/rgb-average/comb-explained-2.png') }})

And here, R=0 and B=0.

![visualisation of partitioning of 3A, part 3]({{ url_for('static', filename='img/rgb-average/comb-explained-3.png') }})

To allow for the zero case, we need 3A-1+3 slots to pick from, of which we pick 2. This completes our explanation of the **first part** of the equation.

TODO relate what I'm saying to the equation, so write {3A-1+3}C{2}.

We need the <font color="blue">second part</font> of the equation because in the **first part** we have unwittingly counted the partitionings where R>255, G>255 and B>255. So we have to subtract these. It's the same as the **first part**, except we remove 256 slots that are given to one of R, G or B. The remaining 3A-1+3-256 slots are divided among R, G and B like before, by choosing 2 slots, so we get {3A-1+3-256}C{2}. And we multiply this by 3 because we repeat for each of R, G and B.

![partitioning of 3A when R>255]({{ url_for('static', filename='img/rgb-average/comb-explained-4-r-gt-255.png') }})

The <font color="red">final part</font> of the equation is needed because we double-subtracted the case where more than one value is greater than 255 in the <font color="blue">second part</font>. TODO explain this

![partitioning of 3A when R>255 and G>255]({{ url_for('static', filename='img/rgb-average/comb-explained-5-rg-gt-255.png') }})

Aaaaand that explains the equation for calculating #{RGB values that average to A}. With that, we can also calculate the probability of R taking on any given value and use a weighted random choice to pick our random R. G and B can be determined from a simple random choice after that.

Here is the full algorithm in Python code.

TODO review code and possibly refactor it

    :::python
    def generate_rgb(A):
        # The goal is to generate RGB so that R+G+B=3A, so that the average (R+G+B)/3 = A.
        remaining = 3 * A
        # R can't be smaller than 3A - 2*255. Otherwise, even with G = B = 255, we will have:
        #   R + G + B = (3A - 2*255 - k) + 255 + 255 = 3A - k < 3A. R+G+B=3A.
        # Likewise, R can't be bigger than 3A.
        # We also need to ensure that R is in the range [0, 255].
        R_possible_values = list(range(max(remaining - 2 * 255, 0), min(remaining, 255) + 1))
        rgbs = num_rgbs_that_sum_to(remaining) 
        # Probability distribution of all of the possible R values. To calculate the probability
        # of a given value of R, we consider the number of pairs of G & B that sum up to 3A - R,
        # which varies by the value of R.
        R_probabilities = [num_gbs_that_sum_to(remaining - r) / possible_rgbs for r in R_possible_values]
        R, = random.choices(population=R_possible_values, weights=R_probabilities)
        remaining -= R
        # As we did above for R, need to set bounds on possible values for G so that it's in the range [0, 255].
        G = random.randint(max(remaining - 255, 0), min(remaining, 255))
        remaining -= G
        B = remaining
        return (R, G, B)

    def num_rgbs_that_sum_to(S):
        """Outputs number of RGB tuples that sum to 'S'."""
        K = 3 # R, G, B
        return (comb(remaining + K - 1, K - 1)
            # Remove cases where R, G or B are >255, since
            # these are not allowed.
            - 3 * comb(remaining + K - 1 - 256, K - 1)
            # Re-add the case where >1 of R, G and B are >255, since
            # they were subtracted twice in the previous bit.
            + 3 * comb(remaining + K - 1 - 2 * 256, K - 1))

    def num_gbs_that_sum_to(S):
        return min(S, 255) - max(S - 255, 0) + 1

This code passes the test that checks whether the RGB values it generates have an average of A. Furthermore, we see that R, G and B have the same distribution of values for A=1.

![distribution of values for R, G, B (fixed)]({{ url_for('static', filename='img/rgb-average/value-distribution-fixed.png') }})

And, more reassuringly, all of the possible RGB tuples for A=245 seem to have an equal probability of being generated (although you can never say for certain with probability-based algorithms).

![distribution of RGB values]({{ url_for('static', filename='img/rgb-average/rgb-value-distribution.png') }})

### Conclusion
We've battled through some tricky combinatorics and come out the other side with an algorithm that solves the problem. We've also encountered the perils of testing algorithms that have an element of randomness. TODO reword this paragraph

An interesting follow-up would be to see if it's possible to generate random RGB values directly from the sequence of valid values rather than going through the rigmarole of calculating probability distributions. In other words, if there are N possible RGB values, we generate a random number 'k' between 1 and N and pass it to our magic function 'f' so that f(k) outputs the kth valid RGB value.

Another possible follow-up would be to create a generic version of the algorithm that generates random tuples of integers with a given average, with arbitrary bounds on the values of the integers. More formally, the task would be to generate a random tuple V = (V1, V2, ..., Vn) such that average(V) = K, where Bl <= Vi <= Bu and Bl <= K <= Bu. The combinatorics behind this would be similar to what we did above.
