title: Solving All Possible Versions of Harry Potter's Potions Puzzle
date: 2019-08-06

In the first book of the Harry Potter series, "Harry Potter and the Philosopher's Stone",
Harry and Hermione must tackle a logic puzzle in order to gain access to the Philosopher's
Stone, because apparently that's what counts as maximum security in the wizarding world.
We're going to take a shot at solving this puzzle ourselves, even though we can't be
sure of its exact parameters.

Here is its text, quoted verbatim from the book.

> Danger lies before you, while safety lies behind,
>
> Two of us will help you, whichever you would find,
>
> One among us seven will let you move ahead,
>
> Another will transport the drinker back instead,
>
> Two among our number hold only nettle wine,
>
> Three of us are killers, waiting hidden in line.
>
> Choose, unless you wish to stay here for evermore,
>
> To help you in your choice, we give you these clues four:
>
> First, however slyly the poison tries to hide
>
> You will always find some on nettle wine’s left side;
>
> Second, different are those who stand at either end,
>
> But if you would move onwards, neither is your friend;
>
> Third, as you see clearly, all are different size,
>
> Neither dwarf nor giant holds death in their insides;
>
> Fourth, the second left and the second on the right
>
> Are twins once you taste them, though different at first sight.

Source: <https://pottermore.fandom.com/wiki/The_Potion_Puzzle>

Now here are its constraints rephrased in a more straight-forward manner:

* There are 7 potions, all of different sizes.
* There are 2 harmless potions.
* There are 3 poisonous potions.
* There is 1 potion that lets us move forward. We need to identify it.
* There is 1 potion that lets us move back. We need to identify it.
* There is at least 1 poisonous potion to the left of both of the harmless potions.
* The potions at the extreme ends are different, neither lets us move forward.
* Neither the biggest nor the smallest potion is poisonous.
* The second potion on the left and the second on the right have the same contents.

The puzzle is not well-defined, given that we don't know where the biggest and smallest
potions are. What we can do is generate all possible versions of the puzzle that match
the above constraints, then solve them.
Hermione didn't have to do any guesswork when she solved the puzzle, which means there
should be a definitive answer, and we can discard any version of the puzzle that has
multiple possible solutions.

TO BE DONE.