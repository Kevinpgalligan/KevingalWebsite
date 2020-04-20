title: Did the Beatles love "you"?
date: 2020-04-01
description: Overanalysing a claim that putting "you" in the first line of a song makes it better.
imgthumbnail: img/beatles-and-you/thumbnail.jpg

On page 3 of *Help! 50 Songwriting, Recording and Career Tips used by the Beatles*, author David Rowley suggests that songwriters should put the word "you" in the first line of their songs:

> Around half of all Lennon-McCartney lyrics use the magic songwriting words 'you' or 'your' in the first line.

> These words, especially when used with the words 'I' or 'me', allow the listener to imagine that the song is about them and allow the listener to imagine that they are singing the song to another person.

For Beatles fans, this raises some incredibly unimportant questions.

* Exactly how many Beatles songs refer to "you" in the first line?
* Are there any other words that appear often in Beatles songs?
* Does "you" lead to some measurable increase in song quality, or is the author of this book blowing hot steam out of his [honey pie](https://www.youtube.com/watch?v=l-ekNlk5VDM)?

To answer these questions, we'll do some analysis on the lyrics of Lennon-McCartney (because who cares about Ringo), and we'll see how the use of "you" (a.k.a. the second person perspective) has affected the success of songs in the Billboard music charts.

<img src="{{ url_for('static', filename='img/beatles-and-you/sad-ringo.jpeg') }}"
     alt="Ringo Starr with a sad expression on his face in black & white."
     class="centered">

### How many Lennon-McCartney songs are addressed to "you"?
I downloaded the lyrics of the Beatles' "core catalogue" studio albums ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/beatles_lyrics_scraper.py)). Instrumental songs, cover songs and songs credited to Ringo Starr / George Harrison were removed manually. This left a total of 138 songs, about 3/4 of the full ~180 songs published by Lennon-McCartney.

Analysis of the lyrics ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/lyrics_analysis.py)) shows that **29%** of the songs contain "you" in the first line (or any of its related forms: "your", "yours" and "yourself"). This is somewhat short of "around half", as claimed by David Rowley, but since **82%** of the songs contain "you" *somewhere* in the lyrics, I'm willing to give him a pass. Lennon-McCartney did, in fact, love "you".

Here's a comparison with some other words. Surprisingly, Lennon-McCartney's love of the second person was outstripped by their love of themselves, as **86%** of lyrics contain the word "I" (or "me", or "my", or "myself").

<img src="{{ url_for('static', filename='img/beatles-and-you/word-freq-beatles.png') }}"
     alt="Percentage of Lennon-McCartney songs that various words appear in; I 86.2%, you 81.9%, love 44.2%, day 23.9%, friend 8.7%, life 7.2%, road 5.1%, guitar 0.7%, mustard 0.7%, beatle 0.0%."
     class="centered">

Other observations: in 138 songs, they never once mentioned the Beatles. Not even in Glass Onion. And roads, or the things that are done in them, are almost as popular a theme as life itself.

### Chart success and you
Lennon-McCartney loved "you", evidently, but can we show that this aspect of their style is worth copying? Does it lead to more successful songs?

To this end, I downloaded the songs in the weekly [Billboard Hot 100](https://en.wikipedia.org/wiki/Billboard_Hot_100) from the start of 1962 until the start of 1970 ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/chart_scrape.py)). That's the active period of the Beatles, roughly speaking, spanning 417 weeks and a total of 5681 unique songs in the charts. On average, ~86% of the songs on the Billboard Hot 100 were actually repeats from previous weeks.

As a measure of success, I assigned a score to each of the songs: 100 points for a week in the #1 position on the charts, 99 points for #2, and so on. For example, a song that charted at #50, #76 and #98 over a period of 3 weeks would receive a total of (101-50)+(101-76)+(101-98) = 51+25+3 = 79 points.

I then gathered the lyrics of 5184 (91.3%) of those songs ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/chart_lyrics_scrape.py), [more code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/google_lyrics_scrape.py), [even more code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/general_lyrics_scrape.py)). The ones I left out were by the Beatles, instrumental, or had lyrics that were difficult to track down on the internet.

Here are the success scores of the songs with lyrics, as a cumulative frequency plot.

<img src="{{ url_for('static', filename='img/beatles-and-you/success-scores-cumulative.png') }}"
     alt="Cumulative frequency graph of song success, as per the metric described above."
     class="centered">

From left to right, the curve increases as the success score overtakes more and more songs. The curve starts off with a sharp rise, gobbling up songs that barely made it onto the chart, such as the slightly creepy Father Knows Best by the Radiants: "Whenever I'm in trouble I ask my dad / For his advice / ... / He said to make your girl love ya / ... / You got to get tough / And treat her wrong / And if she cries, so what / She's trying to put you to a test". It then has a long tail of extremely successful and enduring songs, such as Everyday People by Sly & The Family Stone and...

...[Wipe Out](https://www.youtube.com/watch?v=p13yZAjhU0M)!? That's right. Wipe Out is, by our metric, and among the songs we have selected, the most successful song of the 60s, at a total of 2019 points. And its 1-line lyrics are a mocking rejection of the second person perspective: "Ah ha ha ha ha ha ha ha ha ha ha ha, wipe out!". The 60s certainly were wild.

Analysing the lyrics of all the songs, we see that Lennon-McCartney were not at all exceptional in their love of the second person perspective. **27%** of the songs contain "you" in the first line, while **88%** contain "you" somewhere in the lyrics.

Finally, to see if there's a relationship between use of the second person perspective and chart success, let's look at the distribution of success scores for songs that contain "you" (n=4564) versus songs that don't (n=620).

<img src="{{ url_for('static', filename='img/beatles-and-you/you-vs-notyou-boxplot.png') }}"
     alt="box plot comparing success scores of songs containing you vs songs that don't; the distributions are similar but with the 'not you' songs having a slightly higher median / middle value."
     class="centered">

The coloured boxes show the range of success scores you would be left with if you dropped the 25% least successful and 25% most successful songs. The horizontal dark blue line is the median success score, which is better than exactly 50% of songs. The whiskers extending from the boxes represent the full range of the success scores -- except for outliers, which are represented by circles. Wipe Out sits triumphantly above them all.

As it turns out, songs that don't contain "you" have a higher median success score by 13% (278 vs 316), contrary to what we would expect if we believed David Rowley's songwriting tip.

Here's the distribution of songs that contain "you" in the first line (n=1412) versus those that don't (n=3772).

<img src="{{ url_for('static', filename='img/beatles-and-you/youfirstline-vs-notyoufirstline-boxplot.png') }}"
     alt="box plot comparing success scores of songs containing you in the first line vs songs that don't; the distributions are similar, but the songs with you in the first line have a slightly higher median."
     class="centered">

Songs with "you" in the first line actually have a 9% higher median success score than those without (302 vs 276). It seems that there are a number of lower-scoring "you" songs that don't have "you" in the first line, which drag down the median of whichever category they're in.

### Conclusions
We have seen that the second person perspective was overwhelmingly common in 60s song lyrics. More than Lennon-McCartney secret sauce, it was standard songwriting practice. Furthermore, our overanalysis hasn't shown the use of "you" to have a correlation with chart success. Considering that it was used in almost 90% of chart songs, however, there has to be something of secret sauce about it. Chart-hitting 60s songwriters must have known what they were doing, right?

...*right?*

### Fun bonus reads
* [50 Years of Pop Music](https://www.kaylinpavlik.com/50-years-of-pop-music/) (cool data analysis and visualisation of song lyrics)
* [Why are pop songs getting sadder than they used to be?](https://aeon.co/ideas/why-are-pop-songs-getting-sadder-than-they-used-to-be) (sentiment analysis)
* [The 6 Types of Billboard Hits](https://medium.com/@latenightfroyo/the-6-types-of-billboard-hits-e0cb2987abf) (song clustering)
* [The Good Listener: What's More Important, Lyrics Or Music?](https://www.npr.org/sections/allsongs/2013/05/29/187168874/the-good-listener-whats-more-important-lyrics-or-music?t=1585780553905) (relevant NPR article)

### The code
[It's all here](https://github.com/Kevinpgalligan/BeatlesAndYou).
