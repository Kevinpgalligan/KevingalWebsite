title: The darts player who estimated π
date: 2020-12-23
description: A story of darts and Monte Carlo methods.
requires: math code

A student walked into a bar. She had just finished a gruelling computational maths assignment, and it was time for a relaxing game of darts.

She wandered over to a darts board in the corner and started to throw. After hurling a few darts, she stood back to review her efforts. She wasn't a talented darts player, and the darts seemed to distribute themselves across the board without any noticeable pattern. Almost... at random.

<img src="{{ url_for('static', filename='img/darts/board.png') }}"
     alt="Some darts scattered 'randomly' across a dartboard."
     class="centered">

This last thought put her mind to work. If the darts were aimed at random, then it would be like sampling random points from the darts board. And couldn't that sample be used to estimate the area of the circular part of the board? For example, if 7 out of 10 darts landed in the circle, she would expect the circle to fill about 7/10 of the overall board. And wouldn't that be neat?

But just as she was about to run off and get a measuring tape, another thought occurred to her. Since the area of a circle is $\pi r^2$, wouldn't her estimate also give a free estimate of $\pi$? If the backboard, a square, had sides of length $1$, then its area would be $A=1\times1=1$, and the circle would have radius $r=1/2$. And if a fraction $f$ of her darts hit the circle, her estimate of the area would be $fA=f=\pi (1/2)^2=\pi/4$. Her estimate of $\pi$ would then be:

$$\pi=4f.$$

An estimate of the most famous mathematical constant, and she wouldn't need anything besides darts!

She began to throw. 10 darts, 20 darts, 50 darts. She kept a diligent count of where the darts landed. She stopped when her arm got sore, after she had thrown 100 darts. 81 darts had landed in the circle, which gave an estimate of $\pi = 4(81/100) = 3.24$. It was in the same ballpark as $\pi$, but it seemed like she would need to throw lots more darts to get a decent estimate. And her arm was already dead from throwing. If only there were a way to automate this tedious process.

But of course! That's what she was studying computational maths for. She raced home to her computer and wrote the following code to do the dirty work. She occupied an alternative universe where Common Lisp was the most popular programming language, so that's the programming language she used. Her program took a single parameter, `n`, and simulated `n` dart throws. The output was an estimate of $\pi$.

    :::lisp
    (defun estimate-pi (n)
      (let ((in-circle-count
              (loop repeat n
                    sum (if (apply #'is-in-circle-p (random-xy)) 1 0))))
        (* 4 (/ in-circle-count throws))))

    (defun random-xy ()
      (loop repeat 2
            collect (- 1/2 (random (* 2.0 1/2)))))

    (defun is-in-circle-p (x y)
      (<= (sqrt (+ (square x) (square y))) 1/2))

    (defun square (x)
      (* x x))

She simulated 1000 throws through her Common Lisp interpreter.

    :::lisp
    > (estimate-pi 1000)
    3.104

Wonderful! She would merely have to keep increasing the number of throws and her estimate would get better and better. Soon she'd have the most accurate estimate of π in the world.

    :::lisp
    > (estimate-pi 10000)
    3.1356
    > (estimate-pi 100000)
    3.14144
    > (estimate-pi 1000000)
    3.142252
    > (estimate-pi 10000000)
    3.1426332

Except, after simulating 10 *million* throws, her approximation was still only accurate to 2 decimal places! Her computer's fan was getting noisy, and she didn't want to push it to the limit. Instead, she sat back and wondered how many darts she would need to throw in order to achieve an accuracy of, say, 4 decimal places. Luckily, there was a high probability that she would be able to figure this out, since she was also taking a course on probability.

She knew that she could model the number of darts to hit the darts board as a [*binomial random variable*](https://en.wikipedia.org/wiki/Binomial_distribution), $X$ -- a random variable being a variable that assumes each of its possible values with a certain probability. In this case, if she threw $n$ darts, then $X$ could take on any value from $0$ to $n$. Her estimate $Y$ could be modelled as $Y=4X/n$, and the error as $E=\vert Y-\pi\vert$.

After all that, she could estimate the probability of the error being less than some small value, $\epsilon$.

$$
\begin{aligned}
P(E < \epsilon) &= 1-2P(Y-\pi < -\epsilon) \\
&= 1-2P(4X/n < \pi-\epsilon) \\
&= 1-2P(X < n(\pi-\epsilon)/4).
\end{aligned}
$$

The tricky part here would be calculating $P(X < n(\pi-\epsilon)$. The probability of a binomial random variable being less than some number $k$ is

$$P(X < k) = \sum^{k-1}_{i=0} {n \choose i}p^i(1-p)^{n-i},$$

which is a pain in the ass to calculate as $k$ becomes very large. Thankfully, however, she knew that binomial random variables with $n$ samples and $p$ not too small or too large can be approximated using a normal distribution with mean $\mu=np$ and standard deviation $\sigma=\sqrt{np(1-p)}$. That's how she produced the following graph, which indicated that an accuracy beyond 3 or 4 decimal places was a hopeless cause.

<img src="{{ url_for('static', filename='img/darts/graph.png') }}"
     alt="Plot of the probability of our approximation having a certain level of accuracy given 'n' throws."
     class="centered">

A few days later, the student was explaining her dart experiment to a friend.

"I see," said the friend. "It seems you've squeezed as much of π as you can out of darts. What will your next project be?"

"Well... have you heard of *e*?"

### Appendix A: background
This story was inspired by exercise 3.5 of the computer science textbook, *Structure and Interpretation of Computer Programs*. It asks you to estimate π using a Monte Carlo algorithm, which just means that you simulate a bunch of dart throws.

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
