title: Solving all possible versions of Harry Potter's potions puzzle
date: 2019-08-06

TODO: explain where riddle comes from, desire to solve it, the fact that there are 42 layouts, rather than solving them all by hand (if that's even possible) we can do it programmatically.

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

This is a key plot element of *Harry Potter and the Philosopher's Stone*, the first book in the Harry Potter fantasy book series (it's not found in the movies). Naturally, the riddle is solved promptly by a pair of 10-year-olds, Harry and Hermione.

If you want to solve it for yourself, you'll notice that there's an
important detail missing: the layout of the potions. Solving the puzzle requires knowing the positions of the "giant" (i.e. the biggest) and "dwarf" (i.e. the smallest) potions. Here, we're going to generate all possible layouts of the potions, then we can take them 1-by-1 and solve them. This will be done using the Python programming language (the full program can be found in the appendix).

### ALL the solutions
There are a total of 42 possible potion layouts: 7 positions for the biggest one, since there are 7 potions, which leaves 6 possible spaces for the smallest one. This gives the total of `7 * 6 = 42` layouts. And here they all are, rendered in their immaculate pixelated glory:

...TODO...

Now let's generate all possible solutions for each layout. A "solution" specifies the contents of all the potions while satisfying the following constraints (reworded from the riddle in plain language): 

* There are 2 harmless potions, 3 poison ones, 1 that lets you move forward and 1 that lets you move backward (Harry and Hermione were trying to identify the latter 2).
* There is a poison potion directly to the left of both of the harmless potions.
* The potions at the extreme ends are different, neither lets us move forward.
* Neither the biggest nor the smallest potion is poisonous.
* The second potion on the left and the second on the right have the same contents.

And here are the solutions of each:

...TODO...

Notice that some of the potions have no solutions, and some have multiple possible solutions. A layout with multiple possible solutions is in fact impossible to solve, because you can't figure out which is the correct one (TODO: clarify).

And so here we have the final N (TODO: number) possible layouts and their solutions:

...TODO...

If we look at the similarities between the valid layouts, we see that all of them have a big
potion or small potion in the second-from-left or second-from-right position. This allows us
to immediately identify the second-from-left and second-from-right potions as being harmless:
<explanation here>. This also gives us 2 poison potions, both to the immediate left of the
harmless potions. The rest is pretty easy to figure out. (TODO refine).

...TODO some other comment to wrap it up, whatever puzzle was in Harry Potter was
one of these, can we figure it out based on Hermione's rambling?...
