title: Solving All Possible Versions of Harry Potter's Potions Puzzle
date: 2019-08-06

Imagine protecting your most valuable possession with the following riddle:

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
      
That's pretty much part of the plot of *Harry Potter and the Philosopher's Stone*, the
first book in the Harry Potter series.  The scene is captured in the pixel art below,
officially sanctioned by JK Rowling as canon. TODO canon makes sense?

![pic-of-the-scene]({{ url_for('static', filename='img/harrypotterpotions.png') }})

Of course, the riddle is promptly solved by
a pair of 10-year-olds, the joke being that adult wizards lack an education in basic logic and
problem-solving. TODO not clear wat the joke is

If you want to solve this puzzle for yourself, however, you'll notice that there's an
important detail missing: the layout of the bottles. The placement of the smallest bottle
and biggest bottle is crucial for solving the puzzle.

That's why I've generated here all possible valid layouts and solutions of the potions puzzle,
presented here for your titillation. I first had to generate the layouts, then take
them 1-by-1 and see how many solutions they had, if any.

A valid layout is one that has exactly 1 possible solution, otherwise we
wouldn't be able to solve it. There are a total of 42 layouts: 7 positions for the biggest
potion, and in each of these positions there are 6 slots remaining for the smallest potion,
which gives our total of `7 * 6 = 42` layouts. And here they all are:

...TODO...

Next, we have to attempt to solve each of the versions of the puzzle.

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

Note: ambiguity in "poison left of nettle wine" clue.

TO BE DONE.