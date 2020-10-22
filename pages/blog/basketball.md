title: Can your granny beat Michael Jordan at basketball? A probabilistic answer
date: 2020-10-22
description: How often a better player of Around the World should beat a weaker player.
draft: yes
requires: math

I played a lot of basketball during lockdown. I had a lot of free time, my parents had a basketball hoop, and I had recently watched The Last Dance, the hit basketball documentary about Michael Jordan and the Chicago Bulls. The time was ripe for my basketball ascendance.

Yet my 70-year-old father still managed to beat me at Around the World. Around the World is a basketball game where your objective is to score from each of several positions around the hoop. And my father beat me at it, despite all my practice, and despite the fact that he had never played much basketball in his life. This made me wonder -- how much better than someone do you need to be in order to beat them consistently at Around the World?

<img src="{{ url_for('static', filename='img/basketball/around-the-world.png') }}"
     alt="A variation of Around the World. 7 shooting positions, evenly spaced in a semi-circle around the hoop."
     class="centered">

Around the World is a race. The winner is the first player to make a shot from each of several positions. In my family variant, we had to make 7 shots, spaced at regulars intervals in a semi-circle around the hoop.

Another way to look at it is that the winner is the person who misses the fewest shots before scoring 7 times. And this can be modelled using what is known as a [Pascal distribution](https://en.wikipedia.org/wiki/Negative_binomial_distribution). Given that a player scores each shot with probability $p$, the probability that they miss $k$ shots before finishing the game is

$$P(X=k)={k+7-1 \choose 7-1}(1-p)^k p^7.$$

Where did this ugly-looking expression come from? $1-p$ is the probability that the player misses a given shot. So $(1-p)^k p^7$ is the probability of a single sequence of shots where the player misses $k$ shots before scoring 7. And there are ${k+7-1 \choose 7-1}$ such sequences. Why that many!? Well, the player makes $k+7$ shots in total. The last shot has to be a success, as that's when the player finishes the game. This leaves $k+7-1$ shots, of which we *choose* $7-1=6$ to be successes. It's the [binomial coefficient formula](https://en.wikipedia.org/wiki/Binomial_coefficient), baby!

Okay, we've hand-wavingly derived the ugly probability formula. What does the distribution look like for different values of $p$? In other words, how many shots should you expect to miss at different skill levels?

* INSERT PLOT WITH 4 SUBPLOTS FOR VARYING P.
* MAKE A REFERENCE TO GRANNY, ME, MICHAEL JORDAN.
* WHAT WE'RE REALLY INTERESTED IN...
* NEXT DERIVATION.
* FINAL GRAPH.
