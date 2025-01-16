title: "Chess Detective: Identifying player profiles from Lichess recaps"
date: 2025-01-15
description: How to find someone's Lichess profile based on their yearly recap.
requires: code
tags: chess python
publish: y

Last year, I posted [an analysis](/blog/chess-ratings.html) of Lichess data, with the aim of figuring out what a "normal" rating fluctuation looks like. It was a garbage piece of work, because I was too lazy to properly clean the data. Another time, I [paired up](/blog/rc8.html) with someone at the Recurse Center to do some baby reverse engineering of the Lichess network protocol.

Today we return to Lichess, that most noble of chess websites, with a different objective in mind. Lichess recently released a summary of the year for each player: how many games they played, who their most-played opponent was, and so on. Spotify Wrapped for chess, basically.

<figure>
<img src="{{ url_for('static', filename='img/chess-detective/lichess-recap.png') }}"
     alt="My Lichess recap for 2024. 2084 games played, 73484 moves played, 8 days and 16 hours spent playing, favourite time control was blitz, Ivanomran most played opponent, 170 puzzles solved, Sicilian Defence Alapin Variation most-played opening as white, Caro-Kann Defence Exchange Variation most-played as black."
     class="centered">
<figcaption>My Lichess Wrapped™.</figcaption>
</figure>

I was struck by the question of whether it's possible to identify someone's profile based on the summary page of their Lichess recap. This turned out to be a lot easier than I expected, as all we really need to know is the most-played opponent and the number of games played.

It all starts with the Lichess API. Lichess offers the following HTTPS endpoint for fetching someone's games:

    https://lichess.org/api/games/user/BLAH?since=1590969600000&until=1591833599999

Here, `BLAH` is the username, while `since` and `until` specify the time period. Those numbers are unix timestamps, in milliseconds -- that is, the number of milliseconds that have passed since January 1st 1970. We can write a convenience function to spit out the unix timestamp for a given date.

    :::python
    from datetime import datetime
	def epoch(s):
        return 1000*int(datetime.strptime(s,'%Y-%m-%d').timestamp())
	
	# REPL test.
    >>> epoch("2024-01-01")
    1704067200000

(This is all Python, by the way).

Next, another convenience function. This one assembles the URL and query string, given a username and range of dates.

    :::python
    def get_lichess_url(name, start_date, end_date):
        return (f"https://lichess.org/api/games/user/{name}"
				f"?since={epoch(start_date)}"
				f"&until={epoch(end_date)}")
    
	>>> get_lichess_url("BlunderJan", "2024-01-01", "2025-01-01")
    'https://lichess.org/api/games/user/BlunderJan?[...]'

And here's a 1-line function to download all of someone's games in a given year, using the `requests` library and the convenience function we wrote before. The return value of this function, `r.content`, should be a bytestring containing the [PGN](https://en.wikipedia.org/wiki/Portable_Game_Notation) (Portable Game Notation) data for all the games.

	:::python
	import requests
	def download_games(name, year):
		r = requests.get(get_lichess_url(name, f"{year}-01-01", f"{year+1}-01-01"))
		return r.content
    
    >>> s = download_games("kpgiskpg", 2024) # it's me!
    >>> s
    b'[Event "Rated bullet game"]\n[Site "https://lichess.org/3sEETUVO"]\n[...]'

At this point we have the PGN data stored in a variable called `s`. The following code reads all the games contained therein and counts how many times the given user (the most-played opponent of our mystery profile) played each opponent. We use the `chess` package to read PGN data.

	:::python
    import chess.pgn
	from io import StringIO
	from collections import defaultdict
    
	# The chess API expects to receive a stream of characters, so first
    # convert the bytestring to a string and then wrap it in a streamy thing.
	stream = StringIO(s.decode("utf-8"))
    # Like a regular dictionary, but if a key (i.e. a username) isn't present
    # when we try to access it, then defaultdict gives it an initial value of 0.
	counts = defaultdict(int)
	while True:
		game = chess.pgn.read_game(stream)
		if game is None:
			break
        # Add 1 to the game count for the player who IS NOT the
        # player whose games we're analysing.
		counts[game.headers["Black"]
			   if game.headers["White"] == "kpgiskpg"
               else game.headers["White"]] += 1

Finally, we create a ranked list of all the opponents based on the number of games played, and display them. This *almost* gives us the result: if user B is user A's most-played opponent, then user A is likely to be high in the ranking of user B's opponents.

	:::python
	>>> ranked = list(sorted(counts.items(), key=lambda p: p[1], reverse=True))
	>>> ranked[:10]
	[("MostPlayedOpponent", 12345),
     ("SecondMostPlayedOpponent", 12344),
     ...]

If we wanted, we could then iterate through these players until we found someone who played exactly the number of games shown in the recap, reusing our `download_games` function to calculate that. I'm going to stop here, however, because... well... let's say, hypothetically, that I was curious to find someone's Lichess profile, and that, hypothetically, at this point in the search, a username turned up near the top of the list that was obviously theirs. While that's just a hypothetical scenario, the last piece of the puzzle is still left as an exercise for the reader.

Moral of the story: if you want to share your Lichess recap without revealing your Lichess profile, then you should obscure the most-played opponent. Another thing you should probably do is add a random offset to all the numbers in the recap, since they could also be used to uniquely identify your profile if someone is obsessed enough to trawl through the entire Lichess userbase.

By the way, this is my 50th blog post! Woohoo! 🎉 Stay tuned for post #51, which will be about graphics livecoding with Common Lisp.
