title: Solving All Possible Versions of Harry Potter's Potions Puzzle
date: 2019-08-06

Imagine protecting your life savings with the following riddle:

      Danger lies before you, while safety lies behind,
      Two of us will help you, whichever you would find,
      One among us seven will let you move ahead,
      Another will transport the drinker back instead,
      Two among our number hold only nettle wine,
      Three of us are killers, waiting hidden in line.
      Choose, unless you wish to stay here for evermore,
      To help you in your choice, we give you these clues four:
      First, however slyly the poison tries to hide
      You will always find some on nettle wine’s left side;
      Second, different are those who stand at either end,
      But if you would move onwards, neither is your friend;
      Third, as you see clearly, all are different size,
      Neither dwarf nor giant holds death in their insides;
      Fourth, the second left and the second on the right
      Are twins once you taste them, though different at first sight.

That's a key element of the plot of *Harry Potter and the Philosopher's Stone*, the
first book in the Harry Potter fantasy book series. The riddle is promptly solved
by a pair of 10-year-olds, naturally. This scene isn't found in the movies, just the books.
(TODO: more coherent).

![pic-of-the-scene]({{ url_for('static', filename='img/harrypotterpotions.png') }})

If you want to solve this puzzle for yourself, you'll notice that there's an
important detail missing: the layout of the potions. The placements of the smallest potion
and biggest potion are crucial in solving the puzzle. (TODO: grammar). Here we're going to
generate all possible layouts and solutions of the potions puzzle, then take them 1-by-1 and
solve them.

### All the Solutions

There are a total of 42 layouts: 7 positions for the biggest potion, and in each of these
positions there are 6 slots remaining for the smallest potion, which gives the total
of `7 * 6 = 42` layouts. And here they all are:

...TODO...

And here are the solutions of each:

...TODO...

Notice that some of the potions have no solutions, and some have multiple possible solutions.
A layout with multiple possible solutions is in fact impossible to solve, because you can't
figure out which is the correct one (TODO: clarify).

And so here we have the final N (TODO: number) possible layouts and their solutions:

...TODO...

If we look at the similarities between the valid layouts, we see that all of them have a big
potion or small potion in the second-from-left or second-from-right position. This allows us
to immediately identify the second-from-left and second-from-right potions as being harmless:
<explanation here>. This also gives us 2 poison potions, both to the immediate left of the
harmless potions. The rest is pretty easy to figure out. (TODO refine).

...TODO some other comment to wrap it up, whatever puzzle was in Harry Potter was
one of these, can we figure it out based on Hermione's rambling?...

### Technical Details
Here's the program: TODO

Here are the constraints rephrased in a more straight-forward manner:

* There are 7 potions, all of different sizes.
* There are 2 harmless potions.
* There are 3 poisonous potions.
* There is 1 potion that lets us move forward. We need to identify it.
* There is 1 potion that lets us move back. We need to identify it.
* There is at least 1 poisonous potion to the left of both of the harmless potions.
* The potions at the extreme ends are different, neither lets us move forward.
* Neither the biggest nor the smallest potion is poisonous.
* The second potion on the left and the second on the right have the same contents.
