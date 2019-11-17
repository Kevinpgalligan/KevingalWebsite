title: "Random RGB values that average to N; or, fun with probability and combinatorics"
date: 2019-11-10
draft: yes

On this episode of Problems That Don't Need to Be Solved, we have a special guest...

> ["Generate a random RGB value where the average of R, G and B is A, A being an integer from 0-255."](https://www.reddit.com/r/computerscience/comments/a1ea1s/create_program_to_generate_random_rgb_values_that/)

In other words, we have to find a random RGB value so that (R+G+B)/3=A. Solving this seemingly simple problem, we'll tackle some tricky [combinatorics](https://en.wikipedia.org/wiki/Combinatorics) and see how difficult it is to test algorithms involving randomness.

### An attempt
The first algorithm that might come to mind is the following. We generate random values for R, G and B one at a time, ensuring at each step that 0<=R,G,B<=255 and that it's still possible to satisfy (R+G+B)/3=A, i.e. R+G+B=3A.

Here it is in Python code.

    :::python
    import random as rd
    def generate_rgb(A):
        remaining = 3*A
        # Generate a random value for R, ensuring that:
        #  1) 0 <= R <= 255
        #  2) R <= 3A
        #  3) R >= 3A - 2*255
        # We need #3 because otherwise, even with G=B=255, we'd
        # have R+G+B<3A and the average of R, G, B would be less than A.
        R = rd.randint(max(remaining - 2*255, 0), min(remaining, 255))
        remaining -= R
        # Similar conditions to satisfy for G.
        G = rd.randint(max(remaining - 255, 0), min(remaining, 255))
        remaining -= G
        B = remaining
        return R, G, B

This approach seems reasonable, and if we run a test that checks whether the resulting RGB values have an average of A, it passes with flying "colours".

    :::python
    A = 1
    rgbs = [generate_rgb(A) for _ in range(100000)]
    print("PASS" if all(sum(rgb) == 3*A for rgb in rgbs) else "FAIL")
    # output: PASS

However, this doesn't give the full picture. Besides ensuring that R+G+B=3A, we also need to check that all of the possible RGB values are equally likely to appear, since the problem statement requires randomness. Graphing the frequency of the possible values, we see that this is not the case.

![distribution of RGB values]({{ url_for('static', filename='img/rgb-average/rgb-value-distribution-broken.png') }})

(3, 0, 0) comes out as the mostly likely value at about 25%. This is because P(R=3) -- the probability that R=3 -- is, according to our algorithm, 1/4. And when R=3, the only remaining valid value we can assign to G & B is 0. So, 1/4 of the time, we'll have R=3 and the only possible RGB value is (3, 0, 0). This gives (3, 0, 0) a disproportionately high chance of being chosen.

### Well, I'll be damned! What's the darn-tootin' solution, then?
The problem is that we can't assign R a value from its range of possible values with equal probability, because some of the values are more likely than others (for A=1, this range is 0-3). For example, in the case where A=1, P(R=3) < P(R=1), because R=3 is only possible when (G,B)=(0,0) (10%), whereas R=1 is possible when (G,B) is any of (0,2), (2,0) or (1,1) (30%).

So, before picking R at random, we have to calculate the probability of each of its possible values. The probability that R=k for some k is given by #{GB values that sum to 3A-k} / #{RGB values that average to A}.

(To clarify: #{blah} is the same as saying "the number of blah").

Let's consider the numerator and the denominator separately. Here's the value of #{GB values that sum to 3A-k}:

![numerator formula, min(3A-k, 255) - max(3A-k-255, 0) + 1]({{ url_for('static', filename='img/rgb-average/equation-1-numerator.png') }})

We subtract the minimum value that G can take from the maximum value it can take, then add 1 because it's an inclusive range (the length of a range of integers A<=i<=B is B-A+1). Note: the number of possible G values is the same as the number of possible GB values, since every G value has only 1 corresponding B that goes with it.

Here's a concrete example: if 3A-k=258, then the maximum value that G can take is 255, which is given by min(3A-k, 255) = min(258, 255) = 255. The minimum value it can take, while still possibly satisfying G+B=3A-k, is 3. This is given by max(3A-k-255, 0) = max(258-255, 0) = max(3, 0) = 3. The final result is 255-3+1 = 253.

Now we need to calculate the denominator of P(R=k): #{RGB values that average to A}. The expression is below. It looks scary, but we'll explain each part in an intuitive manner.

![denominator formula, binom(3A+3-1, 2) - 3 * binom(3A+3-1-256, 2) + 3 * binom(3A+3-1-2\*256, 2)]({{ url_for('static', filename='img/rgb-average/equation-2-denominator.png') }})

The first part, **in black**, is the number of ways to divide 3A between R, G and B. The bracket syntax represents the "binomial coefficient" function, which can also be written as nCk: ["the number of ways to choose an (unordered) subset of k elements from a fixed set of n elements"](https://en.wikipedia.org/wiki/Binomial_coefficient). We have {3A+3-1}C{2}, or "the number of ways to choose 2 things from 3A+3-1 things".

Why do we need this binomial coefficient thing? Let's consider an example where A=2, 3A=6. Imagine that we have the numbers 1-6 lined up with spaces between them. We have to put "partitions" in 2 of the spaces in order to divide 3A=6 between R, G and B, as seen below.

![visualisation of partitioning of 3A]({{ url_for('static', filename='img/rgb-average/comb-explained-1.png') }})

R=2, G=3 and B=1. This is known as the [stars and bars](https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics)) approach to explaining nCk, by the way.

We have 3A-1 = 5 spaces, and we have to insert partitions into 2 of them, which gives 5C2 possible combinations. HOWEVER, we also have to account for the case where any of R, G or B are 0. For that reason, we need 3 extra spaces acting as flags that let us set each of R, G and B to 0.

Here we see the case where G=0.

![visualisation of partitioning of 3A, part 2]({{ url_for('static', filename='img/rgb-average/comb-explained-2.png') }})

And here, R=0 and B=0.

![visualisation of partitioning of 3A, part 3]({{ url_for('static', filename='img/rgb-average/comb-explained-3.png') }})

To conclude: including the zero flags, we need 3A-1+3 spaces to pick from, of which we pick 2. And like that, we've come up with the **first part** of the expression, {3A-1+3}C{2}.

We need the <font color="blue">second part</font> of the expression because, in the **first part**, we have unwittingly counted the partitionings where R>255, G>255 and B>255. So we have to subtract these. The intuition is the same as the **first part**, except we remove 256 numbers that are assigned to one of R, G or B. The remaining 3A-1+3-256 spaces are divided among R, G and B like before, by choosing 2 spaces, which gives {3A-1+3-256}C{2}. And we multiply by 3 for the 3 cases: R>255, G>255 and B>255.

Here's an example where 3A=264. We give R a base value of 256, then partition the remaining values like we did before. If we activated the R=0 flag, then the final value of R would just be 256.

![partitioning of 3A when R>255]({{ url_for('static', filename='img/rgb-average/comb-explained-4-r-gt-255.png') }})

The <font color="red">final part</font> of the expression is needed because, in the <font color="blue">second part</font>, we double-subtracted the case where more than one of R, G and B are greater than 255. We subtracted combinations where R>255, which includes combinations where G>255. We also subtracted combinations where G>255, which includes combinations where R>255. In the end we've double-subtracted the combinations with R>255 as well as G>255.

In this example, 3A=519. We set aside 256 numbers for R and 256 numbers for G, then partition the remaining 7 numbers between R, G & B like before.

![partitioning of 3A when R>255 and G>255]({{ url_for('static', filename='img/rgb-average/comb-explained-5-rg-gt-255.png') }})

There are 3A-k-1-2\*256 spaces, of which we pick 2; there are {3A-k-1-2\*256}C{2} ways to do this, which gives the <font color="red">final part</font> of the expression for the denominator of P(R=k).

Now we can calculate #{RGB values that average to A}, which in turn lets us calculate P(R=k).

This is the key to our final algorithm. We use a weighted random choice to pick R, based on P(R=k). After that, G and B can be determined from an unweighted random choice like in the "first attempt" algorithm.

Here it is in Python code. TODO revise this code

    :::python
    def generate_rgb(A):
        # The goal is to generate RGB so that R+G+B=3A, i.e. (R+G+B)/3=A.
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

This code passes the test that checks whether the RGB values it generates have an average of A. And, unlike the first algorithm, all of the possible RGB values for A=245 seem to have an equal probability of being generated.

![distribution of RGB values]({{ url_for('static', filename='img/rgb-average/rgb-value-distribution.png') }})

### Conclusion
We've battled through the combinatorics and come out the other side with a working algorithm. We've also witnessed the peril of testing algorithms that have an element of randomness.

On the next episode of Problems That Don't Need to Be Solved, we'll have our friend blah on the show who'll talk us through blah. TODO something funny here

### Homework
* Generate random RGB values directly from the sequence of valid values rather than going through the rigmarole of calculating probability distributions. In other words, if there are N possible RGB values, we generate a random number 'k' between 1 and N and pass it to our magic function 'f' so that f(k) outputs the kth valid RGB value.
* Create a generic version of the algorithm that generates random tuples of integers with a given average, with arbitrary bounds on the values of the integers. More formally, the task would be to generate a random tuple V = (V1, V2, ..., Vn) such that average(V) = A, where Bl <= Vi <= Bu and Bl <= A <= Bu. The combinatorics behind this would be similar to what we did above.
