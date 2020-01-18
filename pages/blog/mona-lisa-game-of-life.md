title: Finding Mona Lisa in the Game of Life
date: 2019-12-04
draft: yes

The [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), also known as Life, is like a 2d petri dish. Each grid square in the dish is a cell that can be either alive or dead.

![Evolution of Life for a number of states]({{ url_for('static', filename='img/mona-lisa-gol/some-life.gif') }})

The petri dish changes state according to simple rules: a dead cell comes to life if it has 3 live neighbours. A live cell with more than 3 live neighbours dies due to overcompetition. A live cell with fewer than 2 live neighbours dies due to loneliness. Focusing on any cell in the above animation, you will observe that it lives or dies according to these rules.

Besides resulting in cool-looking patterns, it has been proven that Life can simulate anything that an be done by your computer, whether that's adding numbers or captioning images of cats. Not bad for a petri dish.

What does this have to do with the Mona Lisa? It's easy to take a [Life simulator](https://bitstorm.org/gameoflife/), draw a pretty picture, and then let it run. The dark regions of the picture immediately die off due to overpopulation, leaving an outline, which then further melts away and leaves only hints of the original features.

![Evolution of Life with Mona Lisa picture as starting state]({{ url_for('static', filename='img/mona-lisa-gol/mona-start.gif') }})

A more challenging task is to work **backwards** from a picture, i.e. to find Life states that will eventually turn into that picture. The rules of Life are non-reversible, making it damn hard to find the "parent" state of a given Life state.

In this article, we're going to explore how this task can be accomplished using what are known as "SAT solvers" and see some pretty animations that we can generate with the result. Refer to Steve Buscemi / other things here?

### Life, the Universe and SAT Solvers
[SAT](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem) is an abbreviation of "boolean satisfiability problem". Something something example. This can yield multiple solutions.

SAT problems are NP-complete, buuuuuut that's for random SAT problems. Real-life SAT tends to be more achievable.

Why is this relevant to our problem? Well, we can actually recast the problem of finding the previous state of a given Life state as a SAT problem.

EXAMPLE IMAGE FOR A SINGLE CELL.

This can be written as a boolean expression: blah.

Do this for every cell and we end up with an equation for the whole Life state. EXAMPLE EQUATION (but cut short, obviously).

### Great, let's move on to the pretty pictures
Hold up. While this is nice in theory, there are significant "buts".

The first "but" is that *not all Life states have parents*. These states are known as [gardens of eden](https://en.wikipedia.org/wiki/Garden_of_Eden_(cellular_automaton)). There are no Life states that turn into gardens of eden by following the rules of Life, so if our target picture is a garden of eden, then our search will definitely fail.

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
