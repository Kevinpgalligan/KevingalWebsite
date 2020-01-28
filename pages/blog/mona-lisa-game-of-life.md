title: Finding Mona Lisa in the Game of Life
date: 2020-01-19
draft: yes

The [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) is a 2d, grid-shaped petri dish. Each grid square in the dish is a cell that can be either alive or dead.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/some-life.gif') }}"
     alt="Evolution of Life for a number of states."
     class="centered">

The petri dish changes state according to simple rules:

* A dead cell comes to life if it has 3 adjacent cells (or "neighbours") that are alive, through reproduction.
* A live cell with more than 3 live neighbours dies due to overcompetition.
* A live cell with fewer than 2 live neighbours dies due to loneliness.

Every cell in the above animation lives and dies according to these rules.

Besides resulting in cool-looking patterns, it has been proven that the Game of Life ("Life" for short) can simulate anything that can be done by a computer, whether that's summing numbers or captioning images of cats. Not bad for a petri dish.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/spaceship.gif') }}"
     alt="Spaceship pattern moving along in Life"
     class="centered">

What does this have to do with Mona Lisa? It's easy to load a black & white picture as a Life state, where black pixels are live cells and white pixels are dead cells. This allows us to begin a Life simulation with a state that looks like Mona Lisa. The dark regions die off immediately due to overpopulation, leaving an outline, which then melts away further and leaves only hints of the original picture.


<img src="{{ url_for('static', filename='img/mona-lisa-gol/mona-start.gif') }}"
     alt="Evolution of Life with Mona Lisa picture as starting state"
     class="centered">

This looks kinda cool, but what if we want to find a Life state that eventually, after following the rules of Life for a few rounds, reaches a state that looks like Mona Lisa? This requires working backwards instead of forwards from the target picture, which turns out to be a **much** more difficult problem.

In this article, we're going to explore just how difficult this problem is, and how it can be attempted using what are known as "SAT solvers". We'll then look at animations of flowers, Steve Buscemi, and other objects of interest that we can generate with the solution.

### Life, the Universe and SAT Solvers
We call Life state A the "parent" of state B if A turns into B by following the rules of Life. The reason that it's difficult to find the parent of a state is that the rules of Life are non-reversible. There's no easy way to go from a Life state to its parent, and in fact, it's possible for a state to have multiple parents or even no parents.

What we *can* do is construct a boolean equation that captures the conditions that any parent state of our target state must satisfy, then solve it to find a parent, if a parent exists.

(Note: a boolean equation is an equation where the variables take on true / false values, and where the operators, instead of the pluses and minuses that we usually see, are replaced by boolean operators such as AND and OR. For example, the equation `sour AND (NOT sweet)` is solved by setting `sour:=true` and `sweet:=false`. [Read more here](https://en.wikipedia.org/wiki/Boolean_algebra)).

<img src="{{ url_for('static', filename='img/mona-lisa-gol/scream.gif') }}"
     alt="Evolution of the Scream painting as a Life state"
     class="centered">

In the boolean equation that we construct, each variable corresponds to a cell and the value of the variable indicates the health of the cell. False means that the cell is dead, while true means that it's alive. If we find a set of cell states that cause the equation to evaluate to true, then a state with that configuration of cells is a parent of our target state.

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
             OR ...repeat 82 more times for other valid neighbour combinations))
    OR
    (!b2 AND ((a1 AND a2 AND a3 AND !b1 AND !b3 AND !c1 AND !c2 AND !c3)
              OR ...repeat 55 more times for other valid neighbour combinations))

If we repeat this construction for every cell in the grid and chain them together using ANDs, we end up with an equation that we can solve to find a parent of the target state. And, as it happens, there are many "SAT-solving" programs that search for solutions to boolean equations. Once we have our equation, we can ship it off to our SAT solver of choice and sit back, relaxedly sipping our lattes until it gets back to us with a result.

### Great, let's move on to the pretty pictures
WAIT. While this is nice in theory, there are significant "buts".

The first "but" is that, as we touched on in the previous section, *not all Life states have parents*. Such states are known as [Gardens of Eden](https://en.wikipedia.org/wiki/Garden_of_Eden_(cellular_automaton)). Here's an example of a Garden of Eden from Wikipedia. It's impossible to reach this state from any other 9x33 Life state.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/garden-of-eden-example.png') }}"
     alt="An example of a Garden of Eden"
     class="centered">

If our target picture happens to be a Garden of Eden in Life, then the SAT solver will definitely fail to find a parent, because no parent will exist. And the larger a Life state, the more likely it is to be a Garden of Eden, because it has more sub-sections that can possibly be in impossible configurations.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/cloverleaf.gif') }}"
     alt="Evolution of cloverleaf pattern"
     class="centered">

The second "but" is that, as the number of cells increases, so too does the difficulty of the problem. Trying to generate a SAT equation for ~1800 cells blew up my program by consuming the entire 1GB of memory that was available to it. The time taken to find the parent of a Life state also starts to become prohibitive with more than ~400 cells. [SAT problems](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem), after all, are in the NP-complete class of problems, which means that they are damn hard to solve with current methods.

To demonstrate this, below are the timings I got after running backsearch on random Life states of varying sizes, backsearch being the process of finding a Life state's parent. For the record, my processor is a wimpy i3-8130U 2.20GHz.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/cells-vs-backsearch-time.png') }}"
     alt="Time for backsearch vs number of cells, seems to grow exponentially"
     class="centered">

As a result of these "buts", any pictures we use in this article will be 20x20 or less (<=400 cells). Beyond that, the problems take a long time for my computer to handle, and are often impossible to solve.

Here's the output of backsearch on a modest 13x11 sad face. It manages to find 2 previous states before landing in a Garden of Eden. Interestingly, there's no hint of the sad face in the first state, and not much more in the second state, although the live cells do seem to converge towards their final positions.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/sadface.gif') }}"
     alt="Life state becomes sad face, found using backsearch"
     class="centered">

### The results
We run backsearch on Mona Lisa's face and find a parent state. The parent that we find is a Garden of Eden and bears no resemblance to Mona Lisa. Next, we set up our Life simulation with the parent of Mona Lisa's face as the start state. After evaluating the rules of Life once, the start state transitions to Mona Lisa's face. Here's the result.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/mona.gif') }}"
     alt="Life becomes Mona Lisa"
     class="centered">

A flower. The parent state found by the SAT solver is, once again, a Garden of Eden. It contains a faint outline of the flower.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/flower.gif') }}"
     alt="Life becomes a flower"
     class="centered">

Marilyn Monroe and her Garden of Eden parent.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/marilyn.gif') }}"
     alt="Life becomes Marilyn Monroe"
     class="centered">

Steve Buscemi, whose black & white version looks like a pissed-off <a href="https://www.smbc-comics.com/">SMBC</a> character.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/buscemi.gif') }}"
     alt="Life becomes Steve Buscemi"
     class="centered">

Aaaaand a puppy.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/puppy.gif') }}"
     alt="Life becomes a puppy"
     class="centered">

Unfortunately, besides the sad face, we only ever manage a single successful backsearch before running into a dead end.

### Conclusions
We have seen that it is possible to find parents of Game of Life states, although it's a difficult problem for computers to solve. We could speed up the search by encoding the problem in smaller boolean equations. One such encoding is described in The Art of Computer Programming, Volume 4, Fascicle 6. We could also swap out our SAT solver (MiniSAT) for a faster one. In particular, one that allows for parallel search would reduce the search time significantly on computers with multiple processors.

We have also seen that, with our current approach, the parents found by backsearch barely resemble the target picture and often turn out to be Gardens of Eden. To improve on this and find long chains of Life states that gradually turn into the target picture, we could try to search for multiple parent states and pick the one that a) most resembles the target picture, and b) has some property that's less common in Gardens of Eden (fewer or more clustered live cells, perhaps?).

Finally, the Game of Life is just one of <a href="https://en.wikipedia.org/wiki/Cellular_automaton">many possible rulesets</a> that define the behaviour of petri dishes. It would be interesting to experiment with different rulesets and see if they're more amenable to our goal of evolving pictures.

Here's a parting gift. Run this Life state through a 19x19 <a href="https://www.dcode.fr/game-of-life">Life simulator</a> and see what happens.

<img src="{{ url_for('static', filename='img/mona-lisa-gol/message.png') }}"
     alt="Secret message"
     class="centered">

### Technical details
[Here's the code](https://github.com/Kevinpgalligan/MonaLisaGameOfLife) to run Life simulations, do backsearch and create GIFs. It's all in Common Lisp. The cl-sat library was used as a wrapper to call the MiniSat SAT solver, while the skippy library was used to create GIFs. Credit to the #lispgames IRC community for helping me with my silly questions about Common Lisp.

### Further reading
Some fun stuff I came across while researching this article.

* My first idea was to use evolutionary algorithms for finding patterns in Life, but [this turned out to have been done already](https://pdfs.semanticscholar.org/ba77/59e4d871d09459e3751d110137a8434591f6.pdf) in a paper titled "Generating Interesting Patterns in Conway’s Game of Life Through a Genetic Algorithm" by Alfaro, Mendoza and Tice.
* I then had the idea to look for specific patterns, such as pictures. I wasted a bunch of time on trying to do this using evolutionary algorithms until realising that the problem could be solved directly using SAT. And, as it happens, there is already backsearch software for Life [[1]](https://github.com/flopp/gol-sat)[[2]](https://www.conwaylife.com/forums/viewtopic.php?f=9&t=3247). It doesn't seem to have been 
* Backwards solver #2 (from the previous point) mentions The Art of Computer Science, Volume 4, Fascicle 6 as a source of information and exercises on backsearch in Life. I haven't been able to get my hands on the full text.
* A cool thing: [still life paintings in Life](https://codegolf.stackexchange.com/questions/38573/paint-a-still-life-or-a-moving-one-draw-an-image-in-the-game-of-life).
* Another cool thing: [text & image generator in Life](http://tlrobinson.net/blog/2009/02/game-of-life-generator/).

<p class="tiny"><a href="{{ url_for('static', filename='img/mona-lisa-gol/message.gif') }}">spoiler</a></p>
