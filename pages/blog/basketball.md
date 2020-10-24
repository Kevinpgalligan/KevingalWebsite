title: Can your granny beat Michael Jordan at basketball? A probabilistic answer
date: 2020-10-22
description: How often a better player of Around the World should beat a weaker player.
requires: math
imgthumbnail: img/basketball/thumbnail.jpg

I played a lot of basketball during lockdown. I had free time, there was a basketball hoop outside my house, and I had recently watched The Last Dance, the hit basketball documentary about Michael Jordan and the Chicago Bulls. The time was ripe for my basketball ascendance.

Yet my father still managed to beat me at Around the World. Around the World is a basketball game where your objective is to score from each of several positions around the hoop. And my father beat me at it, despite all my practice, and despite the fact that he had never played much basketball in his life.

This made me wonder -- how much better than someone do you need to be in order to beat them consistently at Around the World? That's the question we're going to answer in this article, using tools from probability. By the end, we'll have produced some cool graphs (if you're into that sort of thing), and we'll have definitively answered the question of whether your granny can beat Michael Jordan at basketball.

<img src="{{ url_for('static', filename='img/basketball/around-the-world.png') }}"
     alt="A variation of Around the World. 7 shooting positions, evenly spaced in a semi-circle around the hoop."
     class="centered">

Around the World is a race. Players take turns shooting from set positions around the hoop. A player's turn ends when they miss a shot. The winner is the first player to score a shot from all positions. In my variant, we had to score 7 shots, spaced at regular intervals in a semi-circle around the hoop (see above).

Another way to look at it is that the winner is the person who misses the fewest shots before scoring 7 times. And this can be modelled using what is known as a [Pascal distribution](https://en.wikipedia.org/wiki/Negative_binomial_distribution). Given that a player scores each shot with probability $p$, the probability that they miss $k$ shots before finishing the game is

$$P(X=k)={k+7-1 \choose 7-1}(1-p)^k p^7.$$

Where did this ugly-looking expression come from? Well, there are ${k+7-1 \choose 7-1}$ sequences of shots ([binomial coefficient formula](https://en.wikipedia.org/wiki/Binomial_coefficient)) where the player misses $k$ shots before finishing. Why that many!? The player takes $k+7$ shots in total. The last shot has to be a success, as that's when the player finishes the game. This leaves $k+7-1$ shots, of which we *choose* $7-1=6$ to be successes. As for $(1-p)^k p^7$, it's the probability of a single sequence of shots where the player misses $k$ shots before scoring 7.

Now that we've hand-wavingly derived the probability formula, what does the distribution look like for different values of $p$? How many shots should you expect to miss at different skill levels?

Here's the distribution when $p=0.1$. This is roughly granny-level shooting ability. A granny musters enough energy to hurl the ball in the general direction of the hoop, and it happens to go in, about 1 in 10 times. On average, it takes more than 60 misses before granny finishes the game (indicated by the red line).

<img src="{{ url_for('static', filename='img/basketball/pascal-p0.1.png') }}"
     alt="The distribution for p=0.1; 9/10 shots are misses. A curve that peaks at around 55 missed shots. The mean is just over 60 missed shots."
     class="centered">

Michael Jordan's average free throw percentage over his career was 83.5% ([ref](https://stats.nba.com/player/893/career/)), so let's look at $p=0.835$. We see that it would be common (3/10 games) for MJ to not miss a single shot, and the other player wouldn't even touch the ball if MJ was the starting player. On average, we'd expect MJ to miss slightly more than 1 shot.

<img src="{{ url_for('static', filename='img/basketball/pascal-p0.835.png') }}"
     alt="The distribution for p=0.835."
     class="centered">

Finally, between the two extremes, here's $p=0.5$. This player misses an average of 7 shots before finishing. It's like me on a good day.

<img src="{{ url_for('static', filename='img/basketball/pascal-p0.5.png') }}"
     alt="The distribution for p=0.5."
     class="centered">

Our goal is to estimate the win probability of one player versus another. Me versus my dad. Michael Jordan versus your granny. Michael Jordan versus Michael Jordan. And so on. To do that, we'll have to define some new variables. Let $W$ be the event of a win for the first player, let $F$ be the number of shots they miss, and let $S$ be the same but for the second player. Also let $p_F$ and $p_S$ be the shot success probabilities of each player. We can now state the probability of the first player winning:

$$
P(W) = \sum_{s=0}^\infty P(S=s) P(F \leq S).
$$

We know the value of $P(S=s)$ from above, except that $p$ is swapped for $p_S$. And we get $P(F \leq s)$ by summing up $P(F=f)$ for all $f \leq s$:

$$
P(F \leq s) = \sum_{f=0}^s P(F=f).
$$

Why $\leq$ and not $<$? This is because the first player wins if they miss the same amount of shots as the second player, as the game ends before the second player can finish their shots.

With that out of the way, here's a heat map of the first player's win probability, for varying values of $p_F$ and $p_S$[^infinite]. It ranges from black (the first player will almost certainly lose) to white (the first player will almost certainly win). When we compare Michael Jordan as the first player ($p_F=0.835$) to a granny as the second player ($p_S=0.1$), the result is blindingly white. Granny doesn't stand a chance.

<img src="{{ url_for('static', filename='img/basketball/heatmap.png') }}"
     alt="Heatmap of win probability of first player. p_F and p_S are varied between 0.1 and 0.9. "
     class="centered">

Another observation: the pixel in the bottom left corner is a reddy orange, which is close to a 50% win rate for the first player. The pixel in the top right corner is an orangey yellow, around 70%. It seems that going first gives a bigger advantage to more skillful players.

Here's similar data in table format, with results rounded to 4 decimal places. $p_F$ increases as you go down the table, while $p_S$ increases as you go left to right.

<div class="cooltablewrap">
<table>
<thead>
<tr>
  <th>p<sub>F</sub>/p<sub>S</sub></th>
  <th>0.1</th>
  <th>0.25</th>
  <th>0.4</th>
  <th>0.5</th>
  <th>0.6</th>
  <th>0.835</th>
</tr>
</thead>
<tbody>
<tr>
  <th>0.1</th>
  <td>0.5059</td>
  <td>0.0374</td>
  <td>0.0032</td>
  <td>0.0007</td>
  <td>0.0002</td>
  <td>0.0</td>
</tr>
<tr>
  <th>0.25</th>
  <td>0.9665</td>
  <td>0.5162</td>
  <td>0.162</td>
  <td>0.0654</td>
  <td>0.0245</td>
  <td>0.0016</td>
</tr>
<tr>
  <th>0.4</th>
  <td>0.9974</td>
  <td>0.8637</td>
  <td>0.5291</td>
  <td>0.3208</td>
  <td>0.1726</td>
  <td>0.0234</td>
</tr>
<tr>
  <th>0.5</th>
  <td>0.9994</td>
  <td>0.9499</td>
  <td>0.7382</td>
  <td>0.54</td>
  <td>0.3514</td>
  <td>0.0738</td>
</tr>
<tr>
  <th>0.6</th>
  <td>0.9998</td>
  <td>0.9833</td>
  <td>0.8742</td>
  <td>0.7311</td>
  <td>0.554</td>
  <td>0.1736</td>
</tr>
<tr>
  <th>0.835</th>
  <td>0.9999</td>
  <td>0.9993</td>
  <td>0.9894</td>
  <td>0.965</td>
  <td>0.9124</td>
  <td>0.6235</td>
</tr>
</tbody>
</table>
</div>

This confirms our observation that going first gives a bigger advantage to stronger players. When a granny plays a granny, the first granny has only a small advantage -- a 50.59% chance of winning. Whereas a Michael Jordan who plays first has a 62.35% chance of beating a second Michael Jordan. The game tends to end in fewer turns at higher levels of play, which makes the extra turn at the start count for more.

Let's say that my father, a mediocre player, has a score rate of 40%. And that I, a slightly less mediocre player, have a score rate of 50%. Then my father would still be expected to beat me in about 3/10 games. I guess this explains my humiliating defeats.

As for the scenario we posed at the beginning of the article: it's almost impossible for a granny to beat Michael Jordan. But there's a chance.

*Find the code behind this article [here](https://github.com/Kevinpgalligan/around-the-world).*

[^infinite]: As you might have guessed, we don't compute the entire infinite sum to generate this graph. We stop after enough terms have been added that the error in the result is small.
