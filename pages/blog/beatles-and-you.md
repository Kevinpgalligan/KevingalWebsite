title: Did the Beatles love "you"?
date: 2020-03-10
draft: yes

On page 3 of *Help! 50 Songwriting, Recording and Career Tips used by the Beatles*, author David Rowley suggests that songwriters should put the word "you" in the first line of their songs:

> Around half of all Lennon-McCartney lyrics use the magic songwriting words 'you' or 'your' in the first line.

> These words, especially when used with the words 'I' or 'me', allow the listener to imagine that the song is about them and allow the listener to imagine that they are singing the song to another person.

For Beatles fans, this incredibly unquantified statement raises some incredibly important questions.

* Exactly how many Beatles songs refer to "you" in the first line?
* Are there any other words that appear often in Beatles songs?
* Does "you" lead to some measurable increase in song quality, or is the author of this book blowing hot steam out of his [honey pie](https://www.youtube.com/watch?v=l-ekNlk5VDM)?

This post will answer these questions. More specifically, we'll do some analysis on the lyrics of Lennon-McCartney (because who cares about Ringo), and we'll see how the use of "you" (aka the second person perspective) has affected the success of songs in the Billboard music charts.

<img src="{{ url_for('static', filename='img/beatles-and-you/sad-ringo.jpeg') }}"
     alt="Ringo Starr with a sad expression on his face in black & white colour."
     class="centered">

### How many Lennon-McCartney songs are addressed to "you"?
I downloaded the lyrics of the Beatles' "core catalogue" studio albums ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/beatles_lyrics_scraper.py)). Instrumental songs, cover songs and songs credited to Ringo Starr / George Harrison were removed manually. This left a total of 138 songs. Admittedly, it's a fair bit short of the full ~180 published by Lennon-McCartney.

Analysis of the lyrics ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/lyrics_analysis.py)) showed that **29%** of the songs contain "you" in the first line (or any of its related forms: "your", "yours" and "yourself"). This is somewhat short of "around half", as claimed by David Rowley, but since **82%** of the songs contain "you" *somewhere* in the lyrics, I'm willing to give him a pass. Lennon-McCartney did, in fact, love "you".

Here's a comparison with some other words. Surprisingly, Lennon-McCartney's love of the second person was outstripped by their love of themselves, as **86%** of lyrics contain the word "I"! (Or "me", or "my", or "myself"). Based on word frequency, it would be an equally valid songwriting tip to always talk about yourself in your songs.

<img src="{{ url_for('static', filename='img/beatles-and-you/word-freq-beatles.png') }}"
     alt="Percentage of Lennon-McCartney songs that various words appear in; I 86.2%, you 81.9%, love 44.2%, day 23.9%, friend 8.7%, life 7.2%, road 5.1%, guitar 0.7%, mustard 0.7%, beatle 0.0%."
     class="centered">

In 138 songs, Lennon-McCartney never once mentioned the Beatles. Not even in Glass Onion. And roads, or the things that are done in them, are almost as popular a theme as life itself.

### Chart success and you
Lennon-McCartney loved "you", evidently, but can we show that this aspect of their style is worth copying? Does it lead to more successful songs?

To this end, I downloaded the songs in the weekly [Billboard Hot 100](https://en.wikipedia.org/wiki/Billboard_Hot_100) from the start of 1962 until the start of 1970 ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/chart_scrape.py)). That's the active period of the Beatles, roughly speaking, spanning 417 weeks and a total of 5681 unique songs in the charts. (Side note: this means that, each week, ~86% of the songs on the Billboard Hot 100 were actually repeats from previous weeks).

I then gathered the lyrics of 5184 of those songs ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/chart_lyrics_scrape.py), [more code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/google_lyrics_scrape.py), [even more code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/general_lyrics_scrape.py)). That's 91.3% of the songs. The remainder were written by the Beatles, instrumental, or obscure enough that the lyrics were difficult to track down on the internet. Special mention goes to the song [Wipeout](https://www.youtube.com/watch?v=p13yZAjhU0M), by The Surfaris, which I almost removed from the dataset when I mistook its single line for a copyright notice: "Ah ha ha ha ha ha ha ha ha ha ha ha, wipe out".

Analysis of the lyrics reveals that Lennon-McCartney were not at all exceptional in their love of the second person perspective: **27%** of the songs contain "you" in the first line, while **88%** contain it somewhere in the lyrics.

As a measure of success, I assigned a score to each song: 100 points for a week in the #1 position on the charts, 99 points for #2, and so on. For example, a song that charted at #50, #76 and #98 over a period of 3 weeks would receive a total of (101-50)+(101-76)+(101-98) = 51+25+3 = 79 points. 

Here's the points distribution of all songs, as a cumulative frequency plot.

<img src="{{ url_for('static', filename='img/beatles-and-you/success-scores-cumulative.png') }}"
     alt="Cumulative frequency graph of song success, as per the metric described above."
     class="centered">

From left to right, the curve increases as the success score overtakes more and more songs. We see that about 40% of songs have a success score of less than 250, while about 85% of songs have a success score of less than 1000. The curve starts off with a sharp rise, gobbling up bad songs that barely made it onto the chart (such as Father Knows Best by the Radiants, which I have affectionately nicknamed The Red Pill Anthem: "Whenever I'm in trouble I ask my dad / For his advice / ... / He said to make your girl love ya / ... / You got to get tough / And treat her wrong / And if she cries, so what / She's trying to put you to a test"). It then has a long tail of extremely successful and enduring songs, such as Everyday People by Sly & The Family Stone and... and...

...Wipe Out!? That's right. Wipe Out is, by our metric, the most successful song of the 60s, at a total of 2019 points. That doesn't include the songs of the most successful band of all time (the Beatles), but still. The 60s were weird.

Anyway, to finish our analysis, let's look at the distribution of success scores for songs that contain "you" (n=4564) versus songs that don't (n=620).

<img src="{{ url_for('static', filename='img/beatles-and-you/you-vs-notyou-boxplot.png') }}"
     alt="box plot comparing success scores of songs containing you vs songs that don't; the distributions are similar but with the 'not you' songs having a slightly higher median / middle value."
     class="centered">

The coloured boxes show the range of success scores you would be left with if you dropped the 25% least successful and 25% most successful songs. The orange line is the median or "middle" success score, which is a measure of the average. The "whiskers" extending from the boxes represent the full range of the success scores -- excluding the extreme values, which are represented by circles. Wipe Out sits triumphantly on top of the whole lot.

Funnily enough, songs that don't contain "you" have a higher median success score by 13% (278 vs 316).

Here's the distribution of songs that contain "you" in the first line (n=1412) versus those that don't (n=3772).

<img src="{{ url_for('static', filename='img/beatles-and-you/youfirstline-vs-notyoufirstline-boxplot.png') }}"
     alt="box plot comparing success scores of songs containing you in the first line vs songs that don't; the distributions are similar, but the songs with you in the first line have a slightly higher median."
     class="centered">

This time, the trend is reversed. The median of songs with "you" in the first line and the median of those without sit at 302 and 276, respectively. That's a 9% difference.

### Conclusions
Use of the second person perspective was overwhelmingly common in the 60s. It wasn't a unique feature of Lennon-McCartney's songwriting. As such, a revised edition of David Rowley's songwriting tip might go:

> Around 82% of all Lennon-McCartney lyrics contain the magic songwriting words 'you' or 'your'. This was typical of most songs in the 60s and there is no quantifiable justification for why you should mimic that.

In fact, our analysis of chart success has shown that use of "you" may, if anything, have *reduced* the success of 60s songs, since the median success score of songs was higher if they *didn't* use the second person perspective anywhere in their lyrics.

As a curious reversal of that, songs with "you" in the first line turned out to have a higher median score than those without. This could suggest that there was a cohort of mediocre songwriters in the 60s who didn't have the imagination to use a grammatical person other than the second person, who dragged down the median success score of whichever category they were in. And they tended not to put "you" in the first line of their songs.

### Other stuff
The code from this article can be found [here](https://github.com/Kevinpgalligan/BeatlesAndYou).

And here are some fun articles based on Billboard Hot 100 song lyrics that I came across during my research:

* [50 Years of Pop Music](https://www.kaylinpavlik.com/50-years-of-pop-music/) (cool data analysis and visualisation)
* [Why are pop songs getting sadder than they used to be?](https://aeon.co/ideas/why-are-pop-songs-getting-sadder-than-they-used-to-be) (sentiment analysis)
* [The 6 Types of Billboard Hits](https://medium.com/@latenightfroyo/the-6-types-of-billboard-hits-e0cb2987abf) (song clustering)
