title: Finding Mona Lisa in the Game of Life
date: 2020-01-19
draft: yes

The [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) is like a 2d, grid-shaped petri dish. Each grid square in the dish is a cell that can be either alive or dead.

![Evolution of Life for a number of states]({{ url_for('static', filename='img/mona-lisa-gol/some-life.gif') }})

The petri dish changes state according to simple rules:

* A dead cell comes to life if it has 3 adjacent cells (or "neighbours") that are alive.
* A live cell with more than 3 live neighbours dies due to overcompetition.
* A live cell with fewer than 2 live neighbours dies due to loneliness.

Picking out any cell in the above animation, you will observe that it lives or dies according to these rules.

Besides resulting in cool-looking patterns, it has been proven that the Game of Life ("Life" for short) can simulate anything that can be done by your computer, whether that's adding numbers or captioning images of cats. Not bad for a petri dish.

What does this have to do with Mona Lisa? It's easy to load a black & white picture as a Life state, where black pixels are live cells and white pixels are dead cells. Effectively, we can simulate a Life state that looks like Mona Lisa. The dark regions of the picture die off immediately due to overpopulation, leaving an outline, which then melts away further and leaves only hints of our precious Mona.

![Evolution of Life with Mona Lisa picture as starting state]({{ url_for('static', filename='img/mona-lisa-gol/mona-start.gif') }})

This looks kinda cool, but what if we want to find a Life state that eventually, after following the rules of Life for a few rounds, turns into Mona Lisa? This requires working backwards from the target picture, and turns out to be a **much** more difficult problem.

In this article, we're going to explore just how difficult this problem is, and how it can be attempted using what are known as "SAT solvers". We'll then look at some animations (that turn into flowers, Steve Buscemi and secret messages) that we can generate with the result.

### Life, the Universe and SAT Solvers
The rules of Life aren't easily reversible. If Life state X transitions to state Y, then we call X the "parent" of Y. If every Life state had a single parent, no more no less, then there would exist a simple function to "reverse" the rules of Life and our problem would be solved. The issue is that a Life state can have multiple parents, or even no parents. States with no parent are known as [Gardens of Eden](https://en.wikipedia.org/wiki/Garden_of_Eden_(cellular_automaton)).

[SAT](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem) is an abbreviation of "boolean satisfiability problem". Given a boolean equation -- that is, an equation with variables that can have true or false values, and that evaluates to true or false -- the goal of SAT is to assign values to the variables so that the equation evaluates to true. For example, the equation `sour AND (NOT sweet)`, is solved by setting `sour` to be true and `sweet` to be false.

Random SAT problems are [difficult for computers to solve](https://en.wikipedia.org/wiki/NP-completeness). But the kind of problems that humans are interested in solving usually have a structure that make them easier to solve, and modern SAT-solving software can often solve SAT problems with thousands of variables and millions of combinations of those variables (TODO: fact check this). And we'll use a SAT solver to solve our very own SAT problem: finding the parent of a Life state.

How is SAT related to Life? We can actually recast the problem of finding the previous state of a given Life state, as a SAT problem. Take the example of a single cell and its neighbours.

      1 2 3
    a o x x
    b x x x
    c x o x


Cell b2 is alive. For this to happen, it must be the case that either:

1. It was alive in the previous state and 2-3 of its neighbours were also alive.
2. It was dead in the previous state and 3 of its neighbours were alive.

This can actually be expressed as a boolean equation. If a variable is set to true in the solution, it means that the corresponding cell was alive in the previous state. If a variable is false, the cell was dead.

    (b2 AND ((a1 AND a2 AND (NOT a3) AND (NOT b1) AND (NOT b3) AND (NOT c1) AND (NOT c2) AND (NOT c3))
             OR (a1 AND (NOT a2) AND a3 AND (NOT b1) AND (NOT b3) AND (NOT c1) AND (NOT c2) AND (NOT c3))
             OR ...))
    OR
    ((NOT b2) AND ((a1 AND a2 AND a3 AND (NOT b1) AND (NOT b3) AND (NOT c1) AND (NOT c2) AND (NOT c3))
                   OR ...))


Notice how we need a separate clause to describe each possible state of the neighbours of b2. If we express the same thing for all cells and chain them together with ANDs, then we end up with a SAT equation that, if solved, will give the cell values of a parent state of our Life state! We can pass this equation to a SAT solver and out will pop the answer to our question about Life. Or, if the SAT solver fails to solve the equation, we will know that we've come across a Garden of Eden state.

### Great, let's move on to the pretty pictures
Hold up. While this is nice in theory, there are significant "buts".

The first "but" is that *not all Life states have parents*. There are no Life states that turn into gardens of eden by following the rules of Life, so if our target picture is a garden of eden, then our search will definitely fail.

The second "but" is that, as the number of cells increases, so too does the difficulty of the problem. Trying to generate a SAT equation for ~1800 cells blew up my program by consuming the entire 1GB of memory that was available to it. The time to find the parent of a Life state also starts to become prohibitive with more than ~400 cells.

![Time for backsearch vs number of cells, seems to grow exponentially]({{ url_for('static', filename='img/mona-lisa-gol/cells-vs-backsearch-time.png') }})

Above is the result of some benchmarking I did with an i3-8130U 2.20GHz processor, where backsearch was attempted for random Life states of varying sizes.

As a result of these issues, any pictures we generate will have to be restricted in size to around 20x20 or less. Beyond that, the problems are too difficult for my computer to handle, and are often impossible to solve.

### The pretty pictures
Due to the low resolution, they're not actually all that pretty. But still pretty cool, right?

INSERT GALLERY HERE.

### Technical details
[The code is here](https://github.com/Kevinpgalligan/MonaLisaGameOfLife).

The simulations and GIFs from this article were created using Common Lisp. The cl-sat library was used as a wrapper to call the minisat SAT solver, while the skippy library was used to create GIFs.

Thanks to the #lispgames IRC community for helping me with my silly Common Lisp questions!

### Further reading
GA to generate interesting patterns in Conway's Game of Life: <https://pdfs.semanticscholar.org/ba77/59e4d871d09459e3751d110137a8434591f6.pdf>

Still life paintings in the Game of Life: <https://codegolf.stackexchange.com/questions/38573/paint-a-still-life-or-a-moving-one-draw-an-image-in-the-game-of-life>

Text & image generator in Game of Life: <http://tlrobinson.net/blog/2009/02/game-of-life-generator/>

SAT-based forwards/backwards solver for Game of Life: <https://github.com/flopp/gol-sat>

Logic Life Search (finding patterns using SAT stuff): <https://www.conwaylife.com/forums/viewtopic.php?f=9&t=3247>
