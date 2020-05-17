title: Solving all 42 versions of the Harry Potter potions puzzle
date: 2019-10-03
description: Brute-force search is applied to a puzzle from Harry Potter.

There's a neat puzzle near the end of *Harry Potter and the Philosopher's Stone* (aka *Sorcerer's Stone*). Harry and Hermione enter a chamber, the entrances are blocked by magic fire, and only by decoding the following riddle will they be able to escape:

> Danger lies before you, while safety lies behind,  
> Two of us will help you, whichever you would find,  
> One among us seven will let you move ahead,  
> Another will transport the drinker back instead,  
> Two among our number hold only nettle wine,  
> Three of us are killers, waiting hidden in line.  
> Choose, unless you wish to stay here for evermore,  
> To help you in your choice, we give you these clues four:  
> First, however slyly the poison tries to hide  
> You will always find some on nettle wine’s left side;  
> Second, different are those who stand at either end,  
> But if you would move onwards, neither is your friend;  
> Third, as you see clearly, all are different size,  
> Neither dwarf nor giant holds death in their insides;  
> Fourth, the second left and the second on the right  
> Are twins once you taste them, though different at first sight.

![Some potions]({{ url_for('static', filename='img/potter-puzzle/some-potions.png') }})

Put simply, they have to figure out which potions are in which bottles.

In this post, we're going to solve all 42 possible versions of the puzzle via programming and create a diagram of the results. A diagram like the picture above, but much bigger.

### Wait, why are there 42 versions?
It's because the positions of the "giant" and "dwarf" potion bottles are not specified. There are 7 possible positions for the giant, and for each of those, there are 6 remaining positions for the dwarf, which gives `7 * 6 = 42`. There's no way to know which one J.K. Rowling had in mind when she wrote the puzzle, unless she retcons it through Twitter. We could pick a random version and have a crack at it, but there would be no guarantee that the version we picked would be solvable. That's why we're performing the public service of solving, or proving unsolvable, all 42 versions.

### JUST SOLVE IT
First, here are the constraints of the puzzle, reworded in plainer terms:

1. There are 2 harmless potions, 3 poison ones, 1 that lets you move forward and 1 that lets you move backward.
2. There is a poison potion directly to the left of both of the harmless potions.
3. The potions at the extreme ends are different, neither lets us move forward.
4. Neither the biggest nor the smallest bottle contains poison potion.
5. The second bottle on the left and the second on the right have the same contents.

How do we tackle it? Consider this version. Note that, as stated in the puzzle, there's 1 bottle smaller than all the others (the dwarf) and 1 bottle bigger than all the others (the giant).

![Example version]({{ url_for('static', filename='img/potter-puzzle/solve-1.png') }})

Let's take the bottles one at a time and try all of their possible contents. This approach is known as brute-force search.

The first bottle, for example, can't contain the move-forward potion because of constraint #3 that we mentioned above. Neither can it contain a harmless potion, due to constraint #2 -- it would be impossible for there to be a poison potion to its left. That leaves us with a poison potion and the move-backward potion as possible contents. We try both of these.

Note: in the pictures that follow, green potions = poison, orange = harmless, blue = move-backward, purple = move-forward.

![Example with first potion filled in]({{ url_for('static', filename='img/potter-puzzle/solve-2.png') }})

![Example with first potion filled in]({{ url_for('static', filename='img/potter-puzzle/solve-3.png') }})

We repeat this process for both of the WIP (work in progress) solutions above, taking the second bottle and trying all valid contents. This gives us:

![Example with second potion filled in]({{ url_for('static', filename='img/potter-puzzle/solve-4a.png') }})

![Example with second potion filled in]({{ url_for('static', filename='img/potter-puzzle/solve-4b.png') }})

![Example with second potion filled in]({{ url_for('static', filename='img/potter-puzzle/solve-4c.png') }})

![Example with second potion filled in]({{ url_for('static', filename='img/potter-puzzle/solve-4d.png') }})

And eventually, by continuing like this and discarding WIP solutions if they reach a state where one of the bottles can't be filled without breaking a constraint, we end up with a single valid solution:

![Solution of example]({{ url_for('static', filename='img/potter-puzzle/some-potions.png') }})

Of course, we weren't guaranteed to find a solution. There might have been no solution, or multiple solutions. Having multiple solutions is equivalent to the puzzle being unsolvable because you can't tell which is the correct one.

Applying our algorithm to all puzzle versions gives us the following solutions. 8 versions of the puzzle are solvable, 8 have no solutions and 26 have multiple solutions.

![All solutions]({{ url_for('static', filename='img/potter-puzzle/potions-puzzle-solutions.png') }})

### A closer look at the solutions
Is there something that all of the solvable variations of the puzzle have in common? Yes! Notice that either the small bottle or the big bottle have to be in 2nd or 6th position. This allows us to deduce that the 2nd and 6th bottles contain harmless potion, due to constraints #4 and #5. Without this deductive step, we can't eliminate the possibility that those bottles contain poison and we end up with multiple possible solutions. The solvable variations also require that the other "special" bottle (small or big) is in 3rd or 4th position. Otherwise, we can't pin down the exact location of the move-forward potion.

### Closing comments
I leave you with a quote I like from the book.

> Hermione let out a great sigh and Harry, amazed, saw that she was smiling, the very last thing he felt like doing. ‘Brilliant,’ said Hermione. ‘This isn’t magic – it’s logic – a puzzle. A lot of the greatest wizards haven’t got an ounce of logic, they’d be stuck in here for ever.’

...but wait! Maybe we can identify the canon version of the puzzle based on dialogue from the book.

> ‘Got  it,’  she  said.  ‘The  smallest  bottle  will  get  us  through  the  black fire – towards the Stone.’ 

> ...

> ‘Which one will get you back through the purple flames?’

> Hermione pointed at a rounded bottle at the right end of the line.

Drats. This still leaves 4 candidates:

![Canonical versions, which match the dialogue from the book]({{ url_for('static', filename='img/potter-puzzle/canonical.png') }})

Get retconning, J.K.

### The code
If you're curious about the code for solving the puzzles / drawing the diagram of solutions, see <a href="https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/experiments/hp.py">here</a>.

### Discussion
* [reddit](https://www.reddit.com/r/programming/comments/deauxe/solving_all_42_versions_of_the_harry_potter/)
* [habr](https://habr.com/ru/post/471856/) (Russian translation)
