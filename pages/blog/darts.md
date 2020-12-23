title: The darts player who estimated π
date: 2020-12-23
description: How you can estimate a mathematical constant using only darts and a dartboard.
requires: math code

A young student walked into a bar. She had just finished a gruelling computational mathematics assignment, and it was time for a relaxing game of darts.

She wandered over to the darts board and started to throw. After clumsily hurling a couple of darts, she stood back to review her efforts. The darts seemed to be distributed across the board without any noticeable pattern. Almost... at random.

<img src="{{ url_for('static', filename='img/darts/board.png') }}"
     alt="Some darts scattered 'randomly' across a dartboard."
     class="centered">

This last thought put her mind to work. She had been doing monte carlo stuff. If the darts were indeed landing at random, then it would be as if she were taking a random sampel from teh square. If she treated it as a 1x1 square, with area 1, then she could estimate the area of the circle by the proportion of darts that landed in the circle. hits/(hits+misses). Then another thought occurred to her. The area of a circle is πr^2; if she knew the area of the circle, she could estimate π! The formula is BLAH. All without using so much as a ruler.

She throws 100 darts. An estimate of.... 3.02 (or whatever). Well, it wasn't bad, but it seemed like she would have to throw an awful lot of darts to get a worthwhile estimate. And her arm was already tired. But wait! Couldn't she program a computer to do this sort of repetitive shit? She had been studying monte carlo simulation.

So she raced home and wrote up the following computer program to do the dirty work. She occupied an alternative universe where Common Lisp was the most popular programming language in the world, so that's the one she used. Her program took a single parameter, `n`, and simulated that many dart throws. The output was an estimate of π.

    :::lisp
    (defparameter +r+ 1/2)

    (defun estimate-pi (n)
      (let ((in-circle-count 0))
        (loop repeat n
              do (destructuring-bind (x y)
                     (random-xy)
                   (when (is-in-circle-p x y)
                     (incf in-circle-count))))
        (* 4 (/ in-circle-count n))))

    (defun random-xy ()
      (loop repeat 2
            collect (- +r+ (random (* 2.0 +r+)))))

    (defun is-in-circle-p (x y)
      (<= (sqrt (+ (square x) (square y)))
          +r+))

    (defun square (x)
      (* x x))

She simulated 1000 throws.

    :::lisp
    CL-USER> (estimate-pi 1000)
    3.104

Wonderful! She would merely have to keep increasing the number of throws and her estimate would get better and better. Soon she'd have the most accurate estimate of π in the world.

    :::lisp
    CL-USER> (estimate-pi 10000)
    3.1356
    CL-USER> (estimate-pi 100000)
    3.14144
    CL-USER> (estimate-pi 1000000)
    3.142252
    CL-USER> (estimate-pi 10000000)
    3.1426332

Except, after simulating 10 *million* throws, her approximation was still only accurate to 2 decimal places! Her computer's fan was getting noisy and she didn't want to tire it out, in the same way that she had become tired out in the bar. She sat back and wondered how many darts she would need to throw in order to achieve an estimate of a particular level of accuracy. Luckily, she was also taking a course on probability, so she knew there was a high probability that she would be able to figure it out.

The other day, she had learned about binomial random variables. Let's say we throw $n$ darts. Let $X$ be the number of darts that hit the dartboard. $X$ is what's known as a binomial random variable. "Random variable" means that it has a random value, while "binomial" means that it's equivalent to a bunch of coin flips. In our case, $X$'s random value is between 0 and $n$, and each "coin flip" has probability $p=\pi/4$ of landing on heads (a.k.a. hitting the dartboard).

WHY SHOULD WE CARE ABOUT THIS RANDOM VARIABLE CRAP? Our end goal here is to calculate the probability of our estimate being within a small distance, $\epsilon$, of π. We can use the power of random variables to calculate this. Just bear with me for one more minute.

Let $Y=4X/n$ be our estimate of π. It's also a random variable. We calculate the probability of $Y$ being within $\epsilon$ of π, like so:

$$
\begin{aligned}
P(-\epsilon < Y-\pi < \epsilon) &= 1-2P(Y-\pi < -\epsilon) \\
&= 1-2P(4X/n < \pi-\epsilon) \\
&= 1-2P(X < n(\pi-\epsilon)/4).
\end{aligned}
$$

Okayyyy, we've expressed the probability of our estimate having a given level of accuracy, in terms of the probability of $X$ being less than some number. And *that* can be estimated by using a normal approximation of the binomial random variable $X$. Binomial RVs with $n$ samples and $p$ near 0.5 can be approximated using a normal distribution with mean $\mu=np$ and standard deviation $\sigma=\sqrt{np(1-p)}$. And that's how we produce the following graph.

<img src="{{ url_for('static', filename='img/darts/graph.png') }}"
     alt="Plot of the probability of our approximation having a certain level of accuracy given 'n' throws."
     class="centered">


"Well... have you heard of *e*?"

### Appendix A: in the real world
The story above was inspired by exercise 3.5 of the computer science textbook, *Structure and Interpretation of Computer Programs*. It asks you to estimate π using a Monte Carlo algorithm, which just means that you simulate throwing a dart a bunch of times.

To see the darts experiment in action, watch [this video](https://www.youtube.com/watch?v=M34TO71SKGk) (Physics Girl, "Calculating Pi with Darts").

### Appendix B: the code, in depth
If you're familiar with programming but not the Common Lisp programming language, this appendix will guide you through the full program for estimating π.

First, we define the radius as a constant. `defparameter` is a keyword for defining variables. `+r+` is the name of the constant, which, as described in the story, has a value of 1/2. By convention, constant names are surrounded by '+'.

    :::lisp
    (defparameter +r+ 1/2)

We next define a simple utility function for squaring a number. `defun` is the keyword for defining a function, `square` is the name of our function, and `x` is the function argument. 

    :::lisp
    (defun square (x)
      (* x x))

In the function body, you'll notice that there's no return statement. That's because the return value of a function is the value of the final expression in the function body, which in this case is `(* x x)`. You'll also notice that the multiplication operator, `*`, comes before its operands. It's treated just like a function, so in the same way that we'd write `(square 5)` with our new function to square the number 5, we write `(* 1 2)` to multiply the numbers 1 & 2.

Now we define a function to generate random x & y co-ordinates within the square, returning a list `(x y)`. The function has no arguments. In the function body, we `repeat` our procedure to generate a random co-ordinate, once for x and once for y, and `collect` the results in a list. The circle is centered at (0,0), so the random values we generate should be between -0.5 and +0.5.

    :::lisp
    (defun random-xy ()
      (loop repeat 2
            collect (- +r+ (random (* 2.0 +r+)))))

We need one last helper function. Co-ordinates x & y lie within a circle if they satisfy:

$$\sqrt{x^2 + y^2} \leq r.$$

Here it is in code. We follow the convention that a function name should end in "-p" if it returns a true or false value, p being short for predicate.

    :::lisp
    (defun is-in-circle-p (x y)
      (<= (sqrt (+ (square x) (square y)))
          +r+))

Finally, the meat of the code! Here's a function to simulate a given number of dart throws and return an estimate of π based on the results, just like in the story. There are some tricky parts here if you haven't used Lisp before, but I'll try to give the gist of what's going on.

    :::lisp
    (defun estimate-pi (throws)
      (let ((in-circle-count 0))
        (loop repeat throws
              do (destructuring-bind (x y)
                     (random-xy)
                   (when (is-in-circle-p x y)
                     (incf in-circle-count))))
        (* 4 (/ in-circle-count throws))))

The first new thing is the `let` statement. All we're doing is defining a new local variable, `in-circle-count`, and "letting" it equal 0. Next, we throw darts, `throws` times. `destructuring-bind` takes the 2-value list returned by `random-xy` and assigns the values to local variables `x` and `y`. *When* these co-ordinates are inside the circle, `incf` adds 1 to our count of circle-hitting darts. After the loop is done, we return our approximation. Simples!

If you want to learn more about Common Lisp, the free book [*Practical Common Lisp*](http://www.gigamonkeys.com/book/) is often recommended to beginners. For Lisp newbies, I prefer *The Little Schemer*, which isn't actually about Common Lisp, but is an entertaining, hands-on introduction to Scheme, another Lisp-like language.
