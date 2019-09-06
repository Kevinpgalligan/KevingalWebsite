title: Solving all possible versions of Harry Potter's potions puzzle
date: 2019-08-06
draft: yes

There's a neat little puzzle near the end of *Harry Potter and the Philosopher's Stone*. Harry and Hermione enter a chamber, the entrances are suddenly blocked by magic fire, and they have to figure out which 2 of 7 magic potions will take them to safety, through the following riddle:

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

(TODO phrase it as "missing information") Since there are 42 possible "versions" of the puzzle, 1 for each possible positioning of the "giant" and "dwarf" potions, it makes sense to solve it programmatically instead of painfully trying to solve all of the versions by hand.

### ALL the solutions
Why are there 42 layouts? 7 positions for the big potion, and in each of those 7 positions, there are 6 remaining positions for the small potion, which gives us `7 * 6 = 42`. And here they are, rendered in their immaculate pixelated glory:

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
