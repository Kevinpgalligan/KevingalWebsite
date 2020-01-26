title: Finding Mona Lisa in the Game of Life
date: 2020-01-19
draft: yes

The [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) is like a 2d, grid-shaped petri dish. Each grid square in the dish is a cell that can be either alive or dead.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/some-life.gif') }}"
     alt="Evolution of Life for a number of states."
     class="centered">

The petri dish changes state according to simple rules:

* A dead cell comes to life if it has 3 adjacent cells (or "neighbours") that are alive, through reproduction.
* A live cell with more than 3 live neighbours dies due to overcompetition.
* A live cell with fewer than 2 live neighbours dies due to loneliness.

Picking out any cell in the above animation, you will observe that it lives or dies according to these rules.

Besides resulting in cool-looking patterns, it has been proven that the Game of Life ("Life" for short) can simulate anything that can be done by your computer, whether that's summing numbers or captioning images of cats. Not bad for a petri dish.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/spaceship.gif') }}"
     alt="Spaceship pattern moving along in Life"
     class="centered">

What does this have to do with Mona Lisa? It's easy to load a black & white picture as a Life state, where black pixels are live cells and white pixels are dead cells. In other words, we can simulate Life that starts off looking like Mona Lisa. The dark regions of the picture die off immediately due to overpopulation, leaving an outline, which then melts away further and leaves only hints of the original shapes.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/mona-start.gif') }}"
     alt="Evolution of Life with Mona Lisa picture as starting state"
     class="centered">

This looks kinda cool, but what if we want to find a Life state that eventually, after following the rules of Life for a few rounds, reaches a state that looks like Mona Lisa? Like a slow reveal, or a way of hiding a secret message. This requires working backwards instead of forwards from the target picture, which turns out to be a **much** more difficult problem.

In this article, we're going to explore just how difficult this problem is, and how it can be attempted using what are known as "SAT solvers". We'll then look at animations of flowers, Steve Buscemi and other objects of interest that we can generate with the solution.

### Life, the Universe and SAT Solvers
We call Life state A the "parent" of state B if A turns into B by following the rules of Life. The reason that it's difficult to find the parent of a state is that the rules of Life are non-reversible. There's no "reverse rule" that we can apply to always go from a state to its parent state. In fact, it's possible for a state to have multiple parents, or even no parents.

What we *can* do is construct a boolean equation that captures the conditions that any parent state of our target state must satisfy, then solve it to find a parent, if a parent exists.

(Note: a boolean equation is an equation where the variables take on true / false values, and where the operators, instead of the pluses and minuses that we usually see, are replaced by boolean operators such as AND and OR. For example, the equation `sour AND (NOT sweet)` is solved by setting `sour=true` and `sweet=false`. [Read more here](https://en.wikipedia.org/wiki/Boolean_algebra)).

<img src="{{ url_for('static', filename='img/mona-lisa-gol/scream.gif') }}"
     alt="Evolution of the Scream painting as a Life state"
     class="centered">

In the boolean equation that we construct, each variable corresponds to a cell and the value of the variable indicates the state of the cell. False means that the cell is dead, true means alive. If we find a set of cell states that satisfy the equation, i.e. cause it to evaluate to true, then a state with that configuration of cells is actually a parent of our target state.

What will the equation look like? Let's consider a 3x3 Life grid as an example.

      1 2 3
    a o x x
    b x x x
    c x o x

The middle cell, b2, is alive. For it to be alive in this state, one of the following must be true:

1. It was alive in the previous state and 2-3 of its neighbours were also alive.
2. It was dead in the previous state and 3 of its neighbours were alive.

Keeping in mind that true means alive and false means dead, this can be translated to a boolean equation in a fairly literal way.

    (b2 AND ((a1 AND a2 AND !a3 AND !b1 AND !b3 AND !c1 AND !c2 AND !c3)
             OR (a1 AND !a2 AND a3 AND !b1 AND !b3 AND !c1 AND !c2 AND !c3)
             OR ...repeat 82 more times for other valid neighbour states))
    OR
    (!b2 AND ((a1 AND a2 AND a3 AND !b1 AND !b3 AND !c1 AND !c2 AND !c3)
                   OR ...repeat 55 more times for other valid neighbour states))

If we repeat this construction for every cell in the grid and chain them together using ANDs, we end up with an equation that we can solve to find a parent of the target state. And, as it happens, there are many "SAT-solving" programs that search for solutions to boolean equations. Once we have our equation, we ship it off to our SAT solver of choice and sit back, relaxedly sipping our lattes until it gets back to us with a result.

### Great, let's move on to the pretty pictures
WAIT. While this is nice in theory, there are significant "buts".

The first "but" is that, as we touched on in the previous section, *not all Life states have parents*. Such states are known as [Gardens of Eden](https://en.wikipedia.org/wiki/Garden_of_Eden_(cellular_automaton)). If our target picture happens to be a Garden of Eden in Life, then the SAT solver will definitely fail, no matter how good it is. And the larger a Life state, the more likely it is to be a Garden of Eden, because it has more sub-sections that can possibly be in impossible configurations.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/cloverleaf.gif') }}"
     alt="Evolution of cloverleaf pattern"
     class="centered">

The second "but" is that, as the number of cells increases, so too does the difficulty of the problem. Trying to generate a SAT equation for ~1800 cells blew up my program by consuming the entire 1GB of memory that was available to it. The time to find the parent of a Life state also starts to become prohibitive with more than ~400 cells. [SAT problems](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem), after all, are in the NP-complete class of problems (which means that they are damn hard to solve with current methods).

To demonstrate this, below are the timings I got after running backsearch of random Life states of varying sizes (backsearch being the process of finding a Life state's parent). For the record, my processor is a wimpy i3-8130U 2.20GHz.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/cells-vs-backsearch-time.png') }}"
     alt="Time for backsearch vs number of cells, seems to grow exponentially"
     class="centered">

As a result of these "buts", any pictures we use in this article will be restricted in size to 20x20 or less (<=400 cells). Beyond that, the problems take a long time for my computer to handle, and are often impossible to solve.

Here's the output of backsearch on a modest 13x11 sad face. It manages to find 2 previous states before landing in a Garden of Eden. Interestingly, there's no hint of the sad face in the first state, and not much more in the second state, although the live cells do seem to converge towards their final positions.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/sadface.gif') }}"
     alt="Life state becomes sad face, found using backsearch"
     class="centered">

### The results
The parent state of Mona Lisa's face is a Garden of Eden that doesn't bear any resemblance to the painting.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/mona.gif') }}"
     alt="Life becomes Mona Lisa"
     class="centered">

Marilyn Monroe, GIF HERE.

A flower, GIF HERE.

A truck, GIF HERE.

A hamster, GIF HERE.

Steve Buscemi, GIF HERE.

### Conclusions
We have seen that it is possible to find parents of Game of Life states, although it's inherently difficult.

A few possibilities come to mind for improving the solution:

* Generate smaller boolean equations that still encode the problem, as described in The Art of Computer Programming, Volume 4, Fascicle 6.
* Use a more efficient SAT solver. MiniSat was used here.
* Search for parent states with fewer live cells. This might reduce the chances that backsearch will end up in a Garden of Eden.
* Search for parent states that look more like their children. This might result in nicer animations that gradually transform into the end result.

That said, we have now played with petri dishes more than enough for the time being.

### Technical details
[The code is here](https://github.com/Kevinpgalligan/MonaLisaGameOfLife).

The simulations and GIFs from this article were created using Common Lisp. The cl-sat library was used as a wrapper to call the MiniSat SAT solver, while the skippy library was used to create GIFs.

Credit to the #lispgames IRC community for helping me with my silly questions about Common Lisp.

### Further reading
Some fun stuff I came across while researching this article.

* My first idea was to use evolutionary algorithms for finding patterns in Life, but [this turned out to have been done already](https://pdfs.semanticscholar.org/ba77/59e4d871d09459e3751d110137a8434591f6.pdf) in a paper titled "Generating Interesting Patterns in Conway’s Game of Life Through a Genetic Algorithm" by Alfaro, Mendoza and Tice.
* I then had the idea to look for specific patterns, such as pictures. I wasted a bunch of time on trying to do this using evolutionary algorithms, until realising that the problem could be solved directly using SAT. And, as it happens, there are already backwards/forwards solvers for Life [[1]](https://github.com/flopp/gol-sat)[[2]](https://www.conwaylife.com/forums/viewtopic.php?f=9&t=3247), although they don't seem to have been applied to the task of finding pictures.
* Backwards solver #2 (from the previous point) mentions The Art of Computer Science, Volume 4, Fascicle 6 as a source of information and exercises on backsearch in Life, although I haven't been able to get my hands on it.
* A cool thing: [still life paintings in Life](https://codegolf.stackexchange.com/questions/38573/paint-a-still-life-or-a-moving-one-draw-an-image-in-the-game-of-life).
* Another cool thing: [text & image generator in Life](http://tlrobinson.net/blog/2009/02/game-of-life-generator/).
