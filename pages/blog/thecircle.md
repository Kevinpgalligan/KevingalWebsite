title: Calculating all possible rankings for The Circle USA Season 6
date: 2024-05-11
description: Figuring out who gave what rank to whom. (And coming out as someone who watches reality TV).
requires: code
tags: pop-culture programming python
publish: y

The Circle is a reality competition show where the contestants communicate solely through text. This allows them to come into the show as a "catfish" and assume a new identity, though most people play as themselves. At the end of each round, they rank each other from first to last and the overall top-rated players become "influencers", which gives them the power to eliminate someone from the game. In the final round, the top-ranked player wins the prize money.

As viewers, we are shown only a subset of the ratings (such as: Player A put Player B in 2nd place), and the final ranking. However, I'm always how exactly the players ranked each other. Who were the backstabbers, who were the game-players, who were the do-gooders? This can be figured out with a little bit of programming.

First we need to encode the final results and the known rankings.

**WARNING: SPOILERS AHEAD FOR THE CIRCLE USA SEASON 6**.

	:::python
	class P:
		OLIVIA = "Olivia"
		KYLE = "Kyle"
		QT = "QT"
		JORDAN = "Jordan"
		LAUREN = "Lauren"

	real_final_rank = [
		P.OLIVIA,
		P.KYLE,
		P.QT,
		P.JORDAN,
		P.LAUREN
	]

	players = set(real_final_rank)

	known_rankings = {
		P.OLIVIA: {
			P.QT: 1,
			P.KYLE: 2
		},
		[...]
	}

Then, for each player, we generate all the rankings they *could* have chosen. For example, based on what we know about Olivia, either of the following rankings are possible:

* QT, Kyle, Jordan, Lauren.
* QT, Kyle, Lauren, Jordan.

Here's some code for doing that. It basically makes a "template" of the possible rankings, like `["QT", "Kyle", None, None]`. Then, using `itertools.permutations(...)`, it fills the `None` spots in the template using all possible orderings of the unassigned players.

	:::python
	possible_rankings_per_player = []
	for player in players:
		player_known_rankings = known_rankings[player]
		possible_rankings = []
		ranking_template = [None for _ in range(len(players)-1)]
		for p, rank in player_known_rankings.items():
			ranking_template[rank-1] = p
		unassigned_players = (players - set([player])) - set(player_known_rankings.keys())

		for perm in itertools.permutations(unassigned_players):
			possible_ranking = ranking_template[:]
			i, j = 0, 0
			while i < len(possible_ranking) and j < len(perm):
				if possible_ranking[i] is None:
					possible_ranking[i] = perm[j]
					j += 1
				i += 1
			possible_rankings.append(possible_ranking)
		possible_rankings_per_player.append(possible_rankings)

Finally, we take all possible *combinations* of all the possible rankings, and see which combinations result in the final ordering we saw in the show. Those combinations are the candidates for the true rankings.

To make this clearer: we take the 2 possible rankings made by Olivia, and combine them with all the possible rankings by QT, and all the possible rankings by Kyle, and so on. This might seem like it would result in too many possibilities, but actually it doesn't, because there are only 5 players and quite a lot of information is known.

Here's the table of rankings we know about.

<div class="cooltablewrap">
<table>
<thead>
  <tr>
    <th>Rank</th>
    <th>Olivia</th>
    <th>Kyle</th>
    <th>QT</th>
    <th>Jordan</th>
    <th>Lauren</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>1</td>
    <td>QT</td>
    <td>Olivia</td>
    <td>Kyle</td>
    <td>Lauren</td>
    <td>Jordan</td>
  </tr>
  <tr>
    <td>2</td>
    <td>Kyle</td>
    <td>?</td>
    <td>?</td>
    <td>?</td>
    <td>?</td>
  </tr>
  <tr>
    <td>3</td>
    <td>?</td>
    <td>Lauren</td>
    <td>Olivia</td>
    <td>?</td>
    <td>?</td>
  </tr>
  <tr>
    <td>4</td>
    <td>?</td>
    <td>?</td>
    <td>?</td>
    <td>QT</td>
    <td>?</td>
  </tr>
</tbody>
</table>
</div>

If there are `N` unknown spots in someone's ranking, then there are `N! = N × (N-1) × ... × 1` ways to fill those spots. That means there are `(2!)^4 × 3! = 96` possible combinations for Season 6. Thankfully, `itertools` comes to the rescue again and we can generate all the combinations using `itertools.product(...)`. For every combination, we calculate the final score of each player, where 1st, 2nd, 3rd and 4th place positions are worth 3, 2, 1 and 0 points, respectively. We then order the players by their scores and check if the final ranking matches the *real* final ranking - e.g. we have to discard any combinations that don't result in Olivia being the winner.

	:::python
	for rankings in itertools.product(*possible_rankings_per_player):
		scores = dict([(player, 0) for player in players])
		for ranking in rankings:
			for i, player in enumerate(ranking):
				scores[player] += len(players)-2-i
		final_rank = list(players)
		final_rank.sort(key=lambda p: scores[p], reverse=True)

		if matches_real_rank(final_rank, scores):
			pass # This is a possible solution, print it out!

The only annoying part is the possibility of ties. A player who tied for 2nd place could be presented on the show as having finished in 2nd place *or* 3rd place, according to the narrative that the producers wanted to create. The `matches_real_rank(...)` function resolves this, basically checking that the players fell within the appropriate range of positions based on who they were tied with.

	:::python
	def matches_real_rank(final_rank, scores):
		rank = 0
		for score in sorted(set(scores.values()), reverse=True):
			ps_with_score = [p for p in final_rank if scores[p] == score]
			for p in ps_with_score:
				i = real_final_rank.index(p)
				if not (rank <= i and i < rank+len(ps_with_score)):
					return False
			rank += len(ps_with_score)
		return True

Finally, running the code shows that there are 5 possibilities for the true rankings. Two of them result in a tie for first place between Olivia & Kyle, which I don't think is what happened, 'cause it would then be pretty unfair for Olivia to get all the prize money.

So, here are the 3 remaining possibilities! I've highlighted in green the positions we know with certainty, including both the ones that were revealed in the show and the ones that are constant across all the possible rankings.

Scenario 1: Olivia (8), Kyle (6), QT (6), Jordan (6), Lauren (4).

<div class="cooltablewrap">
<table>
<thead>
  <tr>
    <th>Rank</th>
    <th>Olivia</th>
    <th>Kyle</th>
    <th>QT</th>
    <th>Jordan</th>
    <th>Lauren</th>
  </tr>
</thead>
<tbody>

<tr><td>1</td>
<td class="incel">QT</td>
<td class="incel">Olivia</td>
<td class="incel">Kyle</td>
<td class="incel">Lauren</td>
<td class="incel">Jordan</td>
</tr>
<tr><td>2</td>
<td class="incel">Kyle</td>
<td class="incel">QT</td>
<td class="incel">Jordan</td>
<td class="incel">Olivia</td>
<td class="incel">Olivia</td>
</tr>
<tr><td>3</td>
<td>Jordan</td>
<td class="incel">Lauren</td>
<td class="incel">Olivia</td>
<td>Kyle</td>
<td>QT</td>
</tr>
<tr><td>4</td>
<td>Lauren</td>
<td class="incel">Jordan</td>
<td class="incel">Lauren</td>
<td>QT</td>
<td>Kyle</td>
</tr>
</tbody>
</table>
</div>

Scenario 2: Olivia (8), Kyle (6), QT (6), Jordan (5), Lauren (5).

<div class="cooltablewrap">
<table>
<thead>
  <tr>
    <th>Rank</th>
    <th>Olivia</th>
    <th>Kyle</th>
    <th>QT</th>
    <th>Jordan</th>
    <th>Lauren</th>
  </tr>
</thead>
<tbody>

<tr><td>1</td>
<td class="incel">QT</td>
<td class="incel">Olivia</td>
<td class="incel">Kyle</td>
<td class="incel">Lauren</td>
<td class="incel">Jordan</td>
</tr>
<tr><td>2</td>
<td class="incel">Kyle</td>
<td class="incel">QT</td>
<td class="incel">Jordan</td>
<td class="incel">Olivia</td>
<td class="incel">Olivia</td>
</tr>
<tr><td>3</td>
<td>Lauren</td>
<td class="incel">Lauren</td>
<td class="incel">Olivia</td>
<td>Kyle</td>
<td>QT</td>
</tr>
<tr><td>4</td>
<td>Jordan</td>
<td class="incel">Jordan</td>
<td class="incel">Lauren</td>
<td>QT</td>
<td>Kyle</td>
</tr>
</tbody>
</table>
</div>

Scenario 3: Olivia (8), Kyle (7), QT (5), Jordan (5), Lauren (5).

<div class="cooltablewrap">
<table>
<thead>
  <tr>
    <th>Rank</th>
    <th>Olivia</th>
    <th>Kyle</th>
    <th>QT</th>
    <th>Jordan</th>
    <th>Lauren</th>
  </tr>
</thead>
<tbody>

<tr><td>1</td>
<td class="incel">QT</td>
<td class="incel">Olivia</td>
<td class="incel">Kyle</td>
<td class="incel">Lauren</td>
<td class="incel">Jordan</td>
</tr>
<tr><td>2</td>
<td class="incel">Kyle</td>
<td class="incel">QT</td>
<td class="incel">Jordan</td>
<td class="incel">Olivia</td>
<td class="incel">Olivia</td>
</tr>
<tr><td>3</td>
<td>Lauren</td>
<td class="incel">Lauren</td>
<td class="incel">Olivia</td>
<td>Kyle</td>
<td>Kyle</td>
</tr>
<tr><td>4</td>
<td>Jordan</td>
<td class="incel">Jordan</td>
<td class="incel">Lauren</td>
<td>QT</td>
<td>QT</td>
</tr>
</tbody>
</table>
</div>

We're left with 2 unknowns:

1. Whether Olivia ranked Lauren and Jordan in 3rd or 4th place.
2. Whether Lauren and Jordan ranked QT and Kyle in 3rd or 4th place.

Of course, I've made a critical assumption about how the scoring system works. The producers could easily tamper with it to achieve the outcome they want. In any case, even if my assumption is correct and there was no tampering, the producers still had to make a decision on how to break ties. Some interesting possibilities: there could've been a 3-way tie for 2nd place, and Lauren could've been tied for 3rd place and not have come dead last.

Another interesting thing to look at is to fix the rankings of all but one player, and then see if that player could've won by making different choices. As far as I can see, QT couldn't have won in any scenario. Kyle could've won in Scenario 3, at least, by ranking Olivia in 3rd or 4th place. At the end of the day, however, I think the most deserving player won, as Olivia (a.k.a. Brandon) managed to stay on good terms with everyone and played with the right mixture of heart and brains.

Full code is [here](https://gist.github.com/Kevinpgalligan/9e64c54b55f5f23408a67098bc83e625).
