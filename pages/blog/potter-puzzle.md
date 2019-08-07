title: Solving All Possible Versions of Harry Potter's Potions Puzzle
date: 2019-08-06

Near the climax of *Harry Potter and the Philosopher's Stone*, Hermione and Harry
have just survived what was possibly the most dangerous game of chess in history. They are battling
through a series of challenges that will bring them to the Philosopher's Stone, a
powerful artifact that provides the user with immortality. As they leave the chamber with the giant
chess board and enter the next, the exits of the new chamber are suddenly blocked by magical flames.
Before them is a table with 7 potions and a puzzle written on a piece of paper.

![pic-of-the-scene]({{ url_for('static', filename='img/harrypotterpotions.png') }})

Here is the text of the puzzle, quoted verbatim from the book.

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

(Source: <https://pottermore.fandom.com/wiki/The_Potion_Puzzle>).

We're going to take a shot at solving this puzzle ourselves, even though we can't be
sure of its exact parameters. By that, I mean we don't know where the biggest and smallest
bottles are located, which means we can't even begin to solve it. What we can do is generate
all possible layouts of the potions, each of which gives a different version of the puzzle. Then
we can solve all of those versions.

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

TO BE DONE.