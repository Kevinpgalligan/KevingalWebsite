title: Yet another Wordle solver
date: 2022-02-07
description: The pile grows.
imgthumbnail: img/wordle/thumbnail.jpg
requires: math
publish: y
tags: probability programming data

Like everyone and their dog, I've been playing Wordle. And like everyone and their dog, I've written a [Wordle solver](https://github.com/Kevinpgalligan/yet-another-wordle-solver). Details below.

But first, a quick introduction to the game. If you haven't come across it yet, [Wordle](https://www.powerlanguage.co.uk/wordle/) is a language puzzle game. You have 6 guesses to find a secret 5-letter word. Every time you make a guess, you are told which letters are in the correct position, which letters are correct but in the wrong position, and which letters are incorrect.

In the example below, our first guess is WORDY. The letters 'R' and 'D' are highlighted in yellow, which means that they're in the secret word somewhere, but not in those positions. The remaining letters ('W', 'O' and 'Y'), are greyed out, which means that they're not in the secret word.

<img src="{{ url_for('static', filename='img/wordle/example.png') }}"
     alt="Wordle example where the first three guesses are WORDY, DRAPE and ELDER. ELDER turns out to be the correct word."
     class="centered">

We use that information about the secret word to form our next guess, DRAPE, which tells us that there's an 'E' in the secret word, in addition to the 'R' and 'D'. Finally, we guess ELDER, which turns out to be the correct word. All the letters are highlighted in green, which means they're all in the correct position.

My Wordle solver (let's call it YAWS for short - Yet Another Wordle Solver) is simple. It keeps a list of possible secret words, starting with all 2315 words in Wordle's dictionary. It removes words from the list after each guess, according to the hints it receives. If the solver knows that the secret word begins with 'e', for example, it can remove all words that don't begin with 'e'.

How does YAWS pick the next word to guess? It picks the word that, on average, rules out as many secret words as possible. For example, if we expect there to be an average of 61 possible secret words left after guessing 'raise', then it's a better guess than 'album', which leaves an average of 254 possible secret words. 'album' tends to rule out fewer possibilities than 'raise'.

Very Slightly More Formally, let's say $`S`$ is the set of possible secret words, $`G`$ is the set of allowed guesses (of which there are around 12 thousand), $`X`$ is the actual secret word (which is random), and $`r(g, X, S)`$ is the new set of possible secret words after we guess the word $`g`$. Note that $`r`$ is also a function of $`X`$ and $`S`$, since it depends on what possibilities are left as well as what the actual secret word is.

The next word picked by YAWS is given by

```math
argmin_{g \in G} E[r(g, X, S)],
```

i.e. the 

Next guess is then `argmin g\inG E[H(X, g)]` (notation needs work, but X is the actual secret word, H(X, S, g) is the new size of the secret set given that the actual secret word is X)

And then `E[H(X, S, g)] = sum x\inS P(X=x)H(X, S, g)`.

Other stuff: greedy algorithm, efficiency of computing the expected value, solver performance, compare to my personal score, mention optimal solutions, data analysis of wordle words (how does it compare to ETAOIN SHRDLU?).

More like EAROT LISNCU, amirite?

<img src="{{ url_for('static', filename='img/wordle/letterfreq.png') }}"
     alt="Most frequent letters in Wordle secret dictionary"
     class="centered">

How do you compare which words have changed the most?

<img src="{{ url_for('static', filename='img/wordle/me-vs-solver.png') }}"
     alt="My performance vs the solver's performance."
     class="centered">

References:

* <https://news.ycombinator.com/item?id=30050231>
* <https://www.poirrier.ca/notes/wordle-optimal/>
