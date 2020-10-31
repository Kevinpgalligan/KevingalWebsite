title: The darts player who estimated π
date: 2020-08-05
description: How you can estimate a mathematical constant using only darts and a dartboard.
draft: yes
requires: math code

TODO: polish the story (crappy bit in the middle where she's explaining pi); split the code part into the FULL PROGRAM + results, and an appendix where the code is explained bit by bit; and polish the mathematical analysis. Also find a more appropriate place to put the physics girl link.

*This article introduces a method for approximating π using darts and a dartboard. There are 3 parts: 1) a story, to introduce the method; 2) a program that implements it; and 3) a short mathematical analysis.*

### The story

A darts player and a mathematician walk into a bar. The mathematician takes a seat and orders cranberry juice, while the darts player starts practising at the dartboard.

After a while, the mathematician looks over. "You're not very good, are you?" she says, as another dart whizzes uselessly into the backboard.

"No," says the darts player miserably. "It's almost like playing bingo."

The mathematician sits up. "Bingo, you say?"

"Yeah, just watch."

The darts player throws a few darts at the board, and sure enough, they seem to distribute themselves across it without any noticeable pattern. Almost... at random.

<img src="{{ url_for('static', filename='img/darts/board.png') }}"
     alt="Some darts scattered 'randomly' across a dartboard."
     class="centered">

The mathematician watches with interest. She takes a sip of her cranberry juice and says, "Listen. You might not be very good at darts, but I have a fun experiment we can try."

She walks over to the chalkboard used for keeping score and grabs a lump of chalk.

"Have you heard of *pi* before?" she asks.

"It's a number, right?" replies the darts player.

"Yep. It's one of the most important mathematical constants. It's the ratio of the circumference of a circle," she draws a circle with the chalk, "to its diameter." And now she drags the chalk in a straight line from the left-most to the right-most point of the circle.

<img src="{{ url_for('static', filename='img/darts/circum-diam.png') }}"
     alt="Illustration of circumference and diameter."
     class="centered">

The darts player looks unimpressed. "And?"

"And," she continues, "using your ability to throw darts in a seemingly random pattern, I believe we can do a reasonable job of estimating pi."

"Uhh... how, exactly?"

"You'll have to throw some darts."

The mathematician takes a dart and throws it at the board. It hits the bull's eye. "Beginner's luck," she says apologetically to the poor darts player. "Actually, you'll have to throw a lot of darts."

She moves back to the chalkboard and begins to write. "If the backboard has a width and height of 1, then its area is 1. This means that the diameter of the dartboard is also 1, and its radius is half that, or r=1/2. And the area of the dartboard, which is a circle, is πr<sup>2</sup>, or π/4."

The darts player is focused intently, struggling to recall maths classes from the distant past.

"The area of the dartboard," continues the mathematician, "over the area of the backboard, gives the fraction of the backboard that is covered by the dartboard. That's just π/4. Do you follow?"

The darts player nods. More or less.

"Excellent. Now, here's the key. If you throw a lot of darts at the board, and I mean a hell of a lot of darts, then we can expect *approximately* a fraction of π/4 of the darts to hit the dartboard instead of the backboard. That's because, due to your unusual gift, each randomly-thrown dart has a probability of π/4 of hitting the dartboard. If you throw N darts, then an average of π/4 times N of them will hit the dartboard."

"And there's our estimate!" she exclaims, raising her arms triumphantly. "The number of darts that hit the dartboard, over the total number of darts thrown, is an approximation of π/4. Multiply by 4 and out pops π!"

"Okay, I think I get it," says the darts player. "Let's get to work."

They begin the experiment. The darts player throws darts with mechanical, quivering inaccuracy. The trajectory of each and every dart is a Michaelangelo of pure, unplanned chaos.

The mathematician stops him after he has thrown 10 darts. She has been keeping count of how many darts have hit the dartboard, and how many have hit the backboard.

She whips out her pocket calculator. "All right, let's do the math. 6/10 darts have hit the dartboard." She types it into the calculator. "Multiplying by 4... gives 2.4."

"Oh," says the darts player glumly. "Pi begins with 3, doesn't it? We got the wrong answer."

"Remember when I said that you'd have to throw *a lot* of darts?"

And so, that's what he does. 10 throws, 50 throws, 100 throws, 500 throws. Sweat breaks out on his forehead and his arm is already complaining. But still he continues, throwing dart after dart while the bartender eyes the barely-touched cranberry juice with disapproval.

1000 throws, 2000 throws. 5000 throws! It seems like he has been throwing forever. The bar is spinning around him. Or he's spinning around the bar. He's not quite sure. But finally, when he's on the verge of physical and emotional collapse, the mathematician, who has diligently kept score the whole time, raises her hand. "I think that's enough."

The darts player collapses into a chair. The mathematician sits down beside him with her calculator. "Let's see. 7-8-5-3," she vocalises, "over 1-0-0-0-0. Times 4. Equals..."

"... 3... point.... 1... 4... yes! Yes, we did it!"

They dance around each other, the darts player's exhaustion replaced by excited glee, the mathematician exploding with nerdy enthusiasm. Even the bartender cracks a smile.

Eventually, after extensive, self-congratulatory back-patting, they settle down into a relaxed stupor. The bartender servers them both a round of cranberry juice, on the house.

"That was fantastic!" says the darts player contentedly. "What can we do next?"

"Well... have you heard of *e*?"

### The code
The story above was inspired by exercise 3.5 of the computer science textbook, *Structure and Interpretation of Computer Programs*. It asks you to estimate π using a Monte Carlo algorithm, which just means that you simulate throwing a dart a bunch of times.

In this section, we'll look at a Common Lisp program that can estimate π. If you know a bit about programming but nothing about Common Lisp, then Appendix A provides a step-by-step walkthrough of the code. If you don't care about programming, you can skip to the next section for some maths fun.

Caveats out of the way, here's the full program. It defines a function, `estimate-pi`, that takes a single parameter `n`. The function returns an estimate of π based on `n` simulated throws of a dart at a dartboard of radius 1/2.

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

Here's how the program can be used in a Common Lisp interpreter.

    :::lisp
    CL-USER> (estimate-pi 1000)
    3.104

Throwing only 1000 darts, we get an approximation of 3.104. Not bad at all! What happens if we increase the number of darts?

    :::lisp
    CL-USER> (estimate-pi 10000)
    3.1356
    CL-USER> (estimate-pi 100000)
    3.14144
    CL-USER> (estimate-pi 1000000)
    3.142252
    CL-USER> (estimate-pi 10000000)
    3.1426332

Hmmm. After throwing 10 MILLION darts, our approximation is still only accurate to 2 decimal places! That's a lot of throws for a kinda rubbish estimate of π.

Now, if we want to figure out how many darts we need to throw in order to (probably) achieve a certain level of accuracy in our estimate, we'll need to delve into probability theory. And that is the topic of the next section.

### The maths
In the real world, to get our approximation accurate to 4 decimal places, we'd have to throw about 1 billion darts. So, believe it or not, throwing darts at a board is a crappy method of estimating π. We'll have to look at some probability theory to understand exactly how crappy it is.

Let's say we throw $n$ darts. Let $X$ be the number of darts that hit the dartboard. $X$ is what's known as a binomial random variable. "Random variable" means that it has a random value, while "binomial" means that it's equivalent to a bunch of coin flips. In our case, $X$'s random value is between 0 and $n$, and each "coin flip" has probability $p=\pi/4$ of landing on heads (a.k.a. hitting the dartboard).

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

### Appendix A: the code, in depth
If you're familiar with programming but not the Common Lisp programming language, this appendix will guide you through the full program for estimating π.

First, we define the radius as a constant. `defparameter` is the keyword for defining variables. `+r+` is the name of the constant, which, as described in the story, has a value of 1/2. By convention, constant names are surrounded by '+'.

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

### Appendix B: in the real world
To see the darts experiment in action, watch [this video](https://www.youtube.com/watch?v=M34TO71SKGk) (Physics Girl, "Calculating Pi with Darts").
