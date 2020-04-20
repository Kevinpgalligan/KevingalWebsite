title: "LOVETRIS: an AI for HATETRIS, the ridiculously hard version of Tetris"
date: 2020-04-17
Description: Greedy search, beam search, Monte Carlo tree search.
draft: yes

[HATETRIS](https://qntm.org/hatetris) is a version of Tetris where you are given the worst piece every time. Here's a short clip of it in action.

<img src="{{ url_for('static', filename='img/lovetris/hatetris.gif') }}"
     alt="20-30 seconds of HATETRIS footage, where I get a bunch of S-pieces and only manage to clear a single line."
     class="centered">

What you might observe:

* There are a lot of S-pieces.
* There's no gravity. But, as the game's creator Sam Hughes puts it, "gravity is the least of your problems".
* Unlike regular Tetris, it's entirely non-random, since you always know exactly which piece you're going to get (the worst one).

The "worst" piece is defined as the one that maximises the minimum tower height achievable by the player. If, depending on where you place it, an S-piece could possibly result in a tower height of 4 or 5, and an L-piece could result in a height of 3 or 4, then HATETRIS will give you an S-piece, because its minimum height of 4 is higher than the L-piece's 3.

The all-time HATETRIS high score, according to Sam's website, is 31 cleared lines. That's quite a bit harder than regular Tetris, where people can clear hundreds of thousands of lines if they're not under time pressure.

I was so bad at HATETRIS, myself, that I decided to develop an AI that could play it for me. I'll describe the design of the AI, which is based on greedy search. If you don't care about the technical details and want to see the AI in action, then skip to the end.

### Recreating HATETRIS
I decided to recreate HATETRIS in my programming language of choice, Common Lisp, rather than piggybacking on the original JavaScript code. JavaScript is slow, I thought, and the faster the code, the more game states the AI would be able to evaluate and the better it would perform.

My code didn't turn out too different to the original ([link](https://github.com/qntm/hatetris)). The most interesting part of the HATETRIS code is where, in order to determine the worst piece, it finds all possible placements of each piece using breadth-first search. Each move - left, right, down and rotate - is applied to the piece's starting position, generating 4 new positions. Then the moves are applied to the 4 new positions to generate a further 16 positions. This continues until we've encountered all possible positions of the piece. We save any positions where the piece is resting on the ground or the tower, as these are valid placements.

<figure>

<img src="{{ url_for('static', filename='img/lovetris/bfs.png') }}"
     alt="Initial position & 4 next positions."
     class="centered">

<figcaption>The first step of breadth-first search, where the piece is rotated, moved right, moved left and moved down. This process is repeated on the next positions.</figcaption>
</figure>


After coding up a graphical interface with the [trivial-gamekit](https://borodust.org/projects/trivial-gamekit/) framework, I had a carbon copy of HATETRIS for testing my implementation and was ready to work on the AI.

<img src="{{ url_for('static', filename='img/lovetris/hatetris-clone.gif') }}"
     alt="Carbon copy of HATETRIS, spinning an S-piece around a bit."
     class="centered">

### Greedy search
When placing a piece, the simplest strategy is to pick the move that clears the most lines. However, since in most cases we won't be able to clear *any* lines, we will be left choosing between all possible moves. This is problematic, because we have no way of telling apart the good moves, which set us up to clear lines in the future, from the bad moves, which bring us closer and closer to the hateful jaws of defeat.

That's where heuristics come in. A heuristic is an estimate of how good a move is. It can be anything you imagine, from the number of lines cleared (more is better) to the height of the tower (shorter is better). Multiple heuristics can be combined as a weighted sum. If L is the number of lines cleared and H is the height of the tower, for example, then the overall heuristic score of a move could be `0.9L-0.3H`. The weights should be tuned in order to achieve the best results, either by hand or by some automated method.

<figure>
<img src="{{ url_for('static', filename='img/lovetris/heuristics.png') }}"
     alt="Visualisations of the heuristics I used: number of lines cleared, bumpiness (difference in height of the tower between columns), sum of tower height in each column, and holes (empty squares in the well that have a filled square above them)."
     class="centered">

<figcaption>Mini demonstration of the heuristics I used, which were borrowed from <a href="https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/">this article</a>. They are: lines cleared (0), bumpiness (1), aggregate height (5) and holes (1).</figcaption>
</figure>

Once we've calculated a heuristic score for each move, all we do is pick the one with the highest score. This is known as greedy search, because we guzzle the most appetising choice without considering alternatives.

The last parameter of greedy search is how far to look ahead. Rather than looking at the immediately available moves, we can calculate the heuristic score for combinations of moves, such as "put the L-piece here, then put the S-piece here". We can look any number of moves ahead, although we're limited by compute power. If we assume that every piece has 17 possible placements, like the S-piece in an empty well, then looking 5 moves ahead gives almost 1.5 million possibilities. Something something takes a lot of time to compute and uses a lot of memory. Looking 4 moves ahead is more feasible, with 83 thousand possibilities.

Here's the result of greedy search with 4 moves of lookahead, after tuning the heuristic weights with a genetic algorithm (as described in Appendix A). Our greedy searcher manages to clear X lines, which is already better than me. (or IS it?)

### Beam search
Search depth. Gets out of control. So we need beam search.

Results of unoptimised beam search with untuned parameters.

Various things to improve! Cached states -- with a search depth of 6 and a beam width of 2, we examine 2^(6+1)=128 states. It turns out that about 90/128 (30%-ish) of the states we examine are duplicates. We're wasting effort by examining states that we've already seen. We can avoid this by keeping track of states and not visiting a state again if we've already seen it.

Profiling (see appendix) / multi-threading to speed it up. Allows us to extend search depth / beam width.

And finally, tune parameters.

BETTER RESULT!?

### Monte Carlo Tree Search
May or may not do this.

### Conclusion
Yea boiiiii.

### Appendix A: tuning heuristic weights
Evolutionary algorithms boiiii.
