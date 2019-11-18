title: "Random RGB values that average to N; or, Pointlessness and Probability"
date: 2019-11-18

On this episode of Problems That Don't Need to Be Solved, we have a doozy from r/computerscience.

> ["Generate a random RGB value where the average of R, G and B is A, A being an integer from 0-255."](https://www.reddit.com/r/computerscience/comments/a1ea1s/create_program_to_generate_random_rgb_values_that/)

In other words, we have to generate a random RGB value so that (R+G+B)/3=A. Solving this seemingly simple problem, we'll learn some neat [COMBINATORICS](https://en.wikipedia.org/wiki/Combinatorics) and [EXISTENTIAL DREAD](https://en.wikipedia.org/wiki/Existential_crisis).

### An attempt
The first algorithm that might come to mind is the following. Generate random values one at a time for R, G and B, ensuring at each step that 0<=R,G,B<=255 and that it's still possible to satisfy (R+G+B)/3=A, i.e. R+G+B=3A. If we always stay within these constraints, then we should end up with a valid RGB value with an average of A.

Here it is in Python code.

    :::python
    import random as rd
    def generate_rgb(A):
        S = 3*A
        # Generate a random value for R, ensuring that:
        #  1) 0 <= R <= 255
        #  2) R <= 3A
        #  3) R >= 3A - 2*255
        # We need #3 because otherwise, even with G=B=255, we'd
        # have R+G+B<3A and the average of R, G, B would be less than A.
        R = rd.randint(max(S - 2*255, 0), min(S, 255))
        S -= R
        # Similar conditions to satisfy for G.
        G = rd.randint(max(S - 255, 0), min(S, 255))
        S -= G
        B = S
        return R, G, B

This approach seems reasonable, and if we run a test that checks whether the resulting RGB values have an average of A, it passes with <font color="red">flyi</font><font color="green">ng c</font><font color="blue">olours</font>.

    :::python
    A = 1
    rgbs = [generate_rgb(A) for _ in range(100000)]
    print("PASS" if all(sum(rgb) == 3*A for rgb in rgbs) else "FAIL")
    # output: PASS

However, this doesn't give us the full picture. Besides ensuring that R+G+B=3A, we also need to check that all of the possible RGB values are equally likely to appear, since the problem statement requires randomness. Graphing the frequency of the possible values, we see that this is not the case.

![distribution of RGB values]({{ url_for('static', filename='img/rgb-average/rgb-value-distribution-broken.png') }})

(3, 0, 0) is the most likely value by far at ~25%. This is because P(R=3) -- the probability that R=3 -- is, according to our algorithm, 1/4. And when R=3, the only valid value we can assign to G & B is 0 (otherwise, R+G+B>3A), forcing an RGB value of (3, 0, 0). Meanwhile, there are multiple possible RGB values for each of R=0, R=1 and R=2 that have to share their 1/4 slices of probability pie. (3, 0, 0) ends up with a disproportionately high probability of being chosen. *TODO this paragraph and the next one are a fuckin' drag, man.*

It was difficult to catch this error. R+G+B=3A can be checked directly, but the randomness of the solution cannot. At least, not easily. We had to run the code 1000s of times and then visualise the output.

### Well, I'll be damned! What's the darn-tootin' solution, then?
The possible values of R are not equally likely. For example, in the case where A=1, P(R=3) is less than P(R=1). R=3 is only possible when (G,B) is (0,0) (10% of the time), whereas R=1 is possible when (G,B) is any of (0,2), (2,0) or (1,1) (30% of the time).

Hence, before picking R at random, we have to calculate the probability of each of its possible values. P(R=r) for some r is given by #{GB values that sum to 3A-r} / #{RGB values that average to A}.

Let's consider the numerator and the denominator separately. Here's the value of #{GB values that sum to 3A-r}:

![numerator formula, min(3A-r, 255) - max(3A-r-255, 0) + 1]({{ url_for('static', filename='img/rgb-average/equation-1-numerator.png') }})

We subtract the minimum value that G can be assigned from the maximum value it can be assigned, then add 1 because it's an inclusive range (the length of a range of integers A<=i<=B is B-A+1). Note: the number of possible G values is the same as the number of possible GB values, since every G value has only 1 corresponding B that goes with it. 

To convince yourself that this is correct, consider A=87, 3A=261 and r=1. We have 3A-r=260. The possible GB values are (255,5), (254,6), ..., (5,255). There are 251 such values. This agrees with the expression above. Repeat for various values of A and r. *TODO merge this paragraph and the previous one, if possible. Make it less verbose.*

Now we need to calculate the denominator of P(R=r): #{RGB values that average to A}. The expression is below. It looks scary, but we'll explain each part in an intuitive manner.

![denominator formula, binom(3A+3-1, 2) - 3 * binom(3A+3-1-256, 2) + 3 * binom(3A+3-1-2\*256, 2)]({{ url_for('static', filename='img/rgb-average/equation-2-denominator.png') }})

The first part, **in black**, is the number of ways to divide 3A between R, G and B. The () syntax represents the "binomial coefficient" function, which can also be written as nCk: ["the number of ways to choose an (unordered) subset of k elements from a fixed set of n elements"](https://en.wikipedia.org/wiki/Binomial_coefficient). Above, we have **{3A+3-1}C{2}**, or "the number of ways to choose 2 things from 3A+3-1 things". *TODO this is a bit of a mess.*

Why do we need this binomial coefficient thing? Let's consider an example where A=2, 3A=6. Imagine that we have the numbers 1-6 lined up with spaces between them. We have to put "partitions" in 2 of the spaces in order to divide 3A=6 between R, G and B, as seen below.

*TODO consistent spacing around '='.*

![visualisation of partitioning of 3A]({{ url_for('static', filename='img/rgb-average/comb-explained-1.png') }})

Here, R=2, G=3 and B=1. We have 3A-1 = 5 spaces, and we have to insert partitions into 2 of them, which gives {3A-1}C{2} possible combinations.

HOWEVER, to complete the **first part** we also have to account for the case where any of R, G or B are 0. For that reason, we need 3 extra spaces acting as flags that let us set each of R, G and B to 0. If we put a partition in one of these spaces, it means that the corresponding R/G/B is set to 0. *TODO repetitive use of the phrase "for each of".*

*TODO consider adding sections to break it up, one for numerator and one for the denominator*

Here we see an illustration of G=0.

![visualisation of partitioning of 3A, part 2]({{ url_for('static', filename='img/rgb-average/comb-explained-2.png') }})

And here, R=0 and B=0.

![visualisation of partitioning of 3A, part 3]({{ url_for('static', filename='img/rgb-average/comb-explained-3.png') }})

To conclude: including the zero flags, we need 3A-1+3 spaces, of which we pick 2. And like that, we can make sense of the **first part** of the expression, **{3A-1+3}C{2}**.

We need the <font color="blue">second part</font> of the expression because, in the **first part**, we unwittingly allowed R>255, G>255 and B>255. The intuition is the same as the **first part**, except we remove 256 numbers and assign them to one of R, G or B. The remaining 3A-1+3-256 spaces are divided among R, G and B like before, by inserting partitions into 2 spaces, and there are <font color="blue">{3A-1+3-256}C{2}</font> ways to do so. And we multiply by 3 for the 3 cases: R>255, G>255 and B>255.

Here's an example where 3A=264. We set aside 256 numbers for R, then partition the remaining ones like we did before. If we "enabled" the R=0 flag, then the final value of R would just be 256.

![partitioning of 3A when R>255]({{ url_for('static', filename='img/rgb-average/comb-explained-4-r-gt-255.png') }})

The <font color="red">final part</font> of the expression is needed because, in the <font color="blue">second part</font>, we double-subtracted the case where more than one of R, G and B are greater than 255. First we subtracted combinations where R>255, which includes combinations where G>255. Then we subtracted combinations where G>255, which includes combinations where R>255. In the end we've double-subtracted the combinations where R>255 as well as G>255.

In this example, A=173 and 3A=519. We set aside 256 numbers for R and 256 numbers for G, then partition the remaining 7 numbers between R, G & B like before.

![partitioning of 3A when R>255 and G>255]({{ url_for('static', filename='img/rgb-average/comb-explained-5-rg-gt-255.png') }})

There are 3A+3-1-2\*256 spaces, of which we pick 2. There are <font color="red">{3A+3-1-2\*256}C{2}</font> ways to do this. Multiply by 3 because there are 3 different pairs of R, G & B that can both be >255, and out pops the <font color="red">final part</font>.

Having derived all 3 parts, we can now calculate #{RGB values that average to A}, which in turn lets us calculate P(R=r). This is the key to our final algorithm. We use a weighted random choice to pick R, based on P(R=r). After that, G and B can be determined from an unweighted random choice like in the "first attempt" algorithm.

Here it is in Python code.

    :::python
    import random as rd
    from scipy.special import comb

    def generate_rgbs(A, n=1):
        S = 3*A
        num_rgbs = num_rgbs_that_sum_to(S)
        # R can't be smaller than 3A - 2*255. Otherwise, even with G = B = 255, we will have:
        #   R + G + B = (3A - 2*255 - blah) + 255 + 255 = 3A - blah < 3A.
        # Similarly, R can't be bigger than 3A.
        # We also need to ensure that R is in the range [0, 255].
        R_possible_values = list(range(max(S - 2 * 255, 0), min(S, 255) + 1))
        R_probabilities = [num_gbs_that_sum_to(S - r) / num_rgbs for r in R_possible_values]
        return [generate_rgb(S, R_possible_values, R_probabilities)
                for _ in range(n)]

    def num_rgbs_that_sum_to(S):
        """Outputs number of RGB tuples that sum to 'S'."""
        K = 3 # R, G, B
        return (comb(S + K - 1, K - 1)
            # Remove cases where R, G or B are >255, since
            # these are not allowed.
            - 3 * comb(S + K - 1 - 256, K - 1)
            # Re-add the case where >1 of R, G and B are >255, since
            # they were subtracted twice in the previous bit.
            + 3 * comb(S + K - 1 - 2 * 256, K - 1))

    def num_gbs_that_sum_to(S):
        return min(S, 255) - max(S - 255, 0) + 1

    def generate_rgb(S, R_possible_values, R_probabilities):
        """Generates R,G,B that sum to S given pre-computed probabilities
        for possible R values."""
        R, = rd.choices(population=R_possible_values, weights=R_probabilities)
        S -= R
        G = rd.randint(max(S - 255, 0), min(S, 255))
        S -= G
        B = S
        return (R, G, B)

Besides generating valid RGB values, all of the possible RGB values for A=245 seem to have an equal probability of being generated.

![distribution of RGB values]({{ url_for('static', filename='img/rgb-average/rgb-value-distribution.png') }})

### Conclusion
We've battled through the combinatorics and come out the other side with a working algorithm. Tune in for the next episode of Problems That Don't Need to Be Solved, where we'll be looking at Rubix Cubes. *TODO something other than Rubix Cubes.*

*TODO Possibly merge "homework" into "conclusion".*

### Homework
* Generate random RGB values directly from the sequence of valid values rather than going through the rigmarole of calculating probability distributions. In other words, if there are N possible RGB values, we generate a random number 'k' between 1 and N and pass it to our magic function 'f' so that f(k) outputs the kth valid RGB value.
* Create a generic version of the algorithm that generates random tuples of integers, with arbitrary bounds on the values of the integers. More formally, the task would be to generate a random tuple V = (V1, V2, ..., Vn) such that (V1+V2+...+Vn)/n = A, where Bl <= Vi, A <= Bu. The combinatorics behind this would be similar to what we did above.
