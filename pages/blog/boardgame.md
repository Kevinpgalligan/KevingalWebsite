title: Board games and Markov chains
date: 2022-08-18
description: Using Markov chains to model simple board games.
imgthumbnail: img/thumbnail.jpg
publish: y
tags: probability
requires: math

Children of a certain age are drawn to mindless board games like Snakes & Ladders, which involve no skill or decision-making or social engineering or anything that a reasonable human might associate with the word "fun". The outcome is determined entirely by chance, in the form of dice rolls. The only pleasure I can imagine being derived from such games is the trickle of dopamine that comes from winning and the schadenfreude that comes from seeing a snake devour your opponent, which in my considered opinion is setting children up to become inveterate gamblers and psychopaths, respectively.

While that may be the case, there may be one redeeming aspect to these games, which is that they can be analysed using simple tools from probability. The state of the game, such as the position of the players, changes each round based on the output of a random number generator, such as a die 🎲. The next state depends only on the random output and the current state. This is a [Markov process](https://en.wikipedia.org/wiki/Markov_chain), and it can be analysed using nothing more than matrix multiplication!

To illustrate what a Markov process (or Markov chain) is, let's consider a simple board game.

<figure>
<img src="{{ url_for('static', filename='img/boardgame/boardgame.png') }}"
     alt="A simple board game with 3 positions (A, B and C) and a 6-sided die."
     class="centered">
<figcaption>The best board game ever.</figcaption>
</figure>


It has 3 squares: the start square A, the middle square B, and the end square C. Each turn, you roll a 6-sided die. If 1 comes up, you move back to A or stay on it if you're already there. If 2-5 come up, you advance to the next square. If 6 comes up, you go straight to C, no matter which square you're currently on. Once you're at C, the game is over and the state doesn't change anymore.

The states of the Markov chain that corresponds to this board game, and the probability of moving from one state to another (the transition probability), can be visualised as follows.

<figure>
<img src="{{ url_for('static', filename='img/boardgame/chain.png') }}"
     alt="The Markov chain corresponding to the example board game. There are 3 states: A, B and C. There are arrows between the states, labelled with the probability of going from one state to another."
     class="centered">
<figcaption>The Markov chain corresponding to that board game.</figcaption>
</figure>

The circles are the states, which represent which square you're on. The arrows are labelled with the transition probability from one state to another. Based on the rules above, the transition probabilities for state A are: 1/6 to A, 4/6 to B, and 1/6 to C. They add up to 1/6 + 1/6 + 4/6 = 1, which makes sense because those are all the possible options and the probability of all possible options must be 1. From state B, there is a 1/6 chance of going back to A and a 5/6 chance of going to C, and 1/6 + 5/6 = 1. Finally, the probability of staying in C once you reach it is 1, because at that point the game is over.

The transition matrix $`P`$ expresses all of these probabilities at once:

```math
P = \left[ {\begin{array}{ccc}
        p_{AA} & p_{BA} & p_{CA} \\
        p_{AB} & p_{BB} & p_{CB} \\
        p_{AC} & p_{BC} & p_{CC} \\
    \end{array} } \right]
  = \left[ {\begin{array}{ccc}
        1/6 & 1/6 & 0 \\
        4/6 & 0 & 0 \\
        1/6 & 5/6 & 1 \\
    \end{array} } \right],
```

where $`p_{ij}`$ is the probability of going from state $`i`$ to state $`j`$. Let's say that $`x^{(k)}`$ is a vector containing the probabilities that you will be in state A, B or C after $`k`$ rounds of the game. Since we start off in state A with 100% probability,

```math
x^{(0)} = \left[ {\begin{array}{c}
    p_A^{(0)} \\ p_B^{(0)} \\ p_C^{(0)} \\
    \end{array} } \right]

    = \left[ {\begin{array}{c}
    1 \\ 0 \\ 0
    \end{array} } \right],
```

where $`p_i^{(k)}`$ is the probability of being in state $`i`$ after $`k`$ rounds. To get the probabilities for the $`k`$-th round, multiply the transition matrix by the probabilities for the $`(k-1)`$-th round:

```math
x^{(k)} = P x^{(k-1)} = P P x^{(k-2)} = ... = P^{k} x^{(0)}
```

for $`k>0`$. So there is a fixed probability of being in any state after $`k`$ rounds of the game, which is determined entirely by matrix multiplication. Why does multiplying by the transition matrix give us the probabilities for the next round? If you multiply it out, you'll see that

```math
x^{(k)} = P x^{(k-1)} = \left[ {\begin{array}{c}
    p_A^{(k-1)}p_{AA} + p_B^{(k-1)}p_{BA} + p_C^{(k-1)}p_{CA} \\
    p_A^{(k-1)}p_{AB} + p_B^{(k-1)}p_{BB} + p_C^{(k-1)}p_{CB} \\
    p_A^{(k-1)}p_{AC} + p_B^{(k-1)}p_{BC} + p_C^{(k-1)}p_{CC} \\
    \end{array} } \right].
```

The probability of being in state A in the $`k`$-th round is the probability that we were in state A in the $`(k-1)`$-th round and stayed in A, plus the probability that we were in state B and transitioned to A, plus the probability that were were in state C and transitioned to A. The same applies to the other states. It alllllll makes sense when you multiply it out.

With these newfound powers, let's analyse our Extremely Interesting Game.

* Specify that the column is the FROM state, the row is the TO state.
* Analysis for example game, first by hand and then with J code.
* Now introduce Grace's game and analyse it.
* This could also be applied to Snakes & Ladders or Monopoly (link to stand-up maths video).
