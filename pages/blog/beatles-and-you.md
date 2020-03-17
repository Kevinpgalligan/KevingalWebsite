title: Did the Beatles love "you"? A quantitative study
date: 2020-03-10
draft: yes

On page 3 of *Help! 50 Songwriting, Recording and Career Tips used by the Beatles*, the author (David Rowley) suggests that songwriters should put the word "you" in the first line of their lyrics:

> Around half of all Lennon-McCartney lyrics use the magic songwriting words 'you' or 'your' in the first line.

> These words, especially when used with the words 'I' or 'me', allow the listener to imagine that the song is about them and allow the listener to imagine that they are singing the song to another person.

For Beatles fans, this incredibly unquantified statement raises some incredibly important questions.

* Exactly how many Beatles songs refer to "you" in the first line?
* Are there any other words that appear often in Beatles songs?
* Does "you" actually lead to more chart success for a song, or is the author of this book blowing hot steam out of a certain unmentionable orifice?

This post will answer these questions. More specifically, we'll do some analysis on the lyrics of Lennon-McCartney (because who cares about Ringo), and we'll see how the use of "you" has affected the success of songs in the Billboard music charts.

<img src="{{ url_for('static', filename='img/beatles-and-you/sad-ringo.jpeg') }}"
     alt="Ringo Starr with a sad expression on his face in black & white colour."
     class="centered">

### How many Lennon-McCartney songs are addressed to "you"?
I downloaded the lyrics ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/beatles-lyrics-scraper.py)) of the Beatles' "core catalogue" studio albums. Instrumental songs, cover songs and songs credited to Ringo Starr / George Harrison were removed manually. This left a total of 138 songs, a bit short of the full ~180 songs published by Lennon-McCartney.

Analysis of the lyrics ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/beatles-analysis.py)) showed that **29%** of the songs contain "you" in the first line (or any of its related forms: "your", "yours" and "yourself"). This is somewhat short of "around half", as claimed by David Rowley, but since **82%** of the songs contain "you" *somewhere* in the lyrics, I'm willing to give him a pass. It shouldn't be controversial to state that Lennon-McCartney did, in fact, love "you".

Surprisingly, however, Lennon-McCartney's love of "you" was outstripped by their love of themselves, as **86%** of songs contain "I" somewhere in the lyrics! (Or "me", or "my", or "myself"). It would be an equally valid songwriting tip to always talk about yourself in your songs. Everyone loves an egomaniac, after all.

Here's a comparison with some other words.

<img src="{{ url_for('static', filename='img/beatles-and-you/word-freq-beatles.png') }}"
     alt="Percentage of Lennon-McCartney songs that various words appear in; I 86.2%, you 81.9%, love 44.2%, day 23.9%, friend 8.7%, life 7.2%, road 5.1%, guitar 0.7%, mustard 0.7%, beatle 0.0%."
     class="centered">

Interestingly, in 138 songs written *for* the Beatles, Lennon-McCartney never once *mentioned* the Beatles. Not even in Glass Onion. And roads, or the things that are done in them, are almost as popular a theme as life itself.

### Chart success and you
Despite Lennon-McCartney's evident love of "you", it would be a stretch to issue a commandment indicating that this signature of their style should be mindlessly copied in order to attain songwriting success. Before that, the least we could do would be to establish a correlation between the use of the word "you" and success in music charts.

I downloaded the names of the songs in the [Billboard Hot 100](https://en.wikipedia.org/wiki/Billboard_Hot_100) from the start of 1962 until the start of 1970 ([code](https://github.com/Kevinpgalligan/BeatlesAndYou/blob/master/chart-scrape.py)). This is, approximately, the active period of the Beatles, spanning 417 weeks and a total of 5681 unique songs in the charts. This means that ~86% of the songs on the Billboard Hot 100 were actually repeats from previous weeks.

Next, I downloaded as many of the lyrics of these songs as possible [[4]](TODO-link-to-script). Blah blah old charts before advent of the internet, lyrics not available, ended up with lyrics for N songs.

TODO scrape lyrics, find correlation coefficient between presence of "you" (and other words... love, me, I) and chart score. Chart score: each week, a song gets 100-n+1 points, where n is its position on the chart. That's one possible scheme, anyway. If there's no correlation, then David Rowley was talking shit. May want to control for the Beatles...is that ensured by repeating the test without Beatles songs?

### Conclusion
* Is it a valid tip?
* Mention that we've only shown a correlation. Experimental study required.
