title: Yet another Wordle solver
date: 2022-02-07
description: Another one for the pile.
imgthumbnail: img/wordle/thumbnail.jpg
publish: y
tags: probability programming data

Like everyone and their dog, I've been playing Wordle. And like everyone and their dog, I've written a [Wordle solver](https://github.com/Kevinpgalligan/yet-another-wordle-solver). Details below.

If you haven't come across it yet, [Wordle](https://www.powerlanguage.co.uk/wordle/) is a language puzzle game. You have 6 guesses to find a secret 5-letter word. Every time you make a guess, you are told which letters are in the correct position, which letters are correct but in the wrong position, and which letters are incorrect. In the example below, the first guess is WORDY. The letters 'R' and 'D' are highlighted in yellow, which means that they're in the secret word somewhere, but not in those positions. The remaining letters ('W', 'O' and 'Y'), are greyed out, which means that they're not in the secret word.

<img src="{{ url_for('static', filename='img/wordle/example.png') }}"
     alt="Wordle example where the first three guesses are WORDY, DRAPE and ELDER. ELDER turns out to be the correct word."
     class="centered">

We take that information about the secret word and make another guess. In this case, our next guess is DRAPE, which tells us that there's an 'E' in the secret word, in addition to the 'R' and 'D'. Finally, we guess ELDER, which turns out to be the correct word. All the letters are highlighted in green, which means they're all in the correct position.

My Wordle solver is simple. It keeps a list of possible secret words, starting with all 2315 words in Wordle's dictionary. It removes words from the list after each guess, according to the hints it receives. If the solver knows that the secret word begins with 'e', for example, it can remove all words that don't begin with 'e'. 

How does it pick the next word to guess? It *minimises the expected number of secret words left after it makes the guess*. For example, if we expect there to be an average of 61 secret words left over after guessing 'raise', then it's a better guess than 'album', which leaves an average of 254 secret words remaining.

Other stuff: mention that there are 12k+ possible guesses, greedy algorithm, efficiency of computing the expected value, solver performance, compare to my personal score, mention optimal solutions, data analysis of wordle words (how does it compare to ETAOIN SHRDLU?).

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
