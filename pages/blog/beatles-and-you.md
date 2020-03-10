title: Did the Beatles love "you"? A quantitative study
date: 2020-03-10
draft: yes

On page 3 of *Help! 50 Songwriting, Recording and Career Tips used by the Beatles*, the author (David Rowley) suggests that songwriters should put the word "you" in the first line of their song lyrics:

> Around half of all Lennon-McCartney lyrics use the magic songwriting words 'you' or 'your' in the first line.

> These words, especially when used with the words 'I' or 'me', allow the listener to imagine that the song is about them and allow the listener to imagine that they are singing the song to another person.

As a Beatles fan, this incredibly unquantified statement brings some incredibly important questions to mind.

* Exactly how many Beatles songs refer to "you" in the first line?
* Are there any other words that appear often in Beatles songs?
* Does "you" actually lead to more chart success for a song, or is the author of this book blowing hot steam out of his unmentionable orifice?

This post will answer these questions. More specifically, we'll run the numbers on the songs of Lennon-McCartney (because who cares about Ringo), and we'll see how the use of "you" has affected the success of songs in the Billboard music charts.

<img src="{{ url_for('static', filename='img/beatles-and-you/sad-ringo.jpeg') }}"
     alt="Ringo Starr with a sad expression on his face in black & white colour."
     class="centered">

### How many Lennon-McCartney songs are addressed to "you"?
I downloaded [[1]](https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/experiments/beatles-and-you/beatles-lyrics-scraper.py) the lyrics of the Beatles' ["core catalogue"](https://en.wikipedia.org/wiki/The_Beatles_discography#Studio_albums) studio albums. Instrumental songs, cover songs and songs credited to Ringo Starr / George Harrison were removed manually. This left a total of 140 songs. A bit short of the full ~180 songs published by Lennon-McCartney, but it should be a representative sample.

Analysis [[2]](https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/experiments/beatles-and-you/beatles-analysis.py) of the lyrics showed that **30%** of the songs had "you" (or "your") in the first line. This is somewhat short of "around half", as claimed by David Rowley, but since **83%** of the songs contained "you" *somewhere* in the lyrics, I'm willing to give him a pass. It shouldn't be controversial to state that Lennon-McCartney did, in fact, love "you".

TODO insert graph of "you" (and other words) over time; compare to frequency of other words in their songs? Or some other comparison of interest, showing that their use of "you" is especially strident compared to general English usage or other writers of English songs. OR, maybe rather than doing it over time, use bar graphs w/ comparisons of various words vs their usage in other mediums.

However, despite Lennon-McCartney's evident love for "you", it would be a stretch to issue a commandment indicating that this signature of their style should be mindlessly copied in order to attain song-writing success. Before that, the least we could do would be to establish a correlation between the use of the word "you" and success in music charts.

### Chart success and you
I downloaded [[3]](TODO-link-to-script) the names of the songs in the [Billboard Hot 100](https://en.wikipedia.org/wiki/Billboard_Hot_100) from the start of 1962 until the start of 1970. This is roughly the active period of the Beatles, spanning 417 weeks and a total of 5681 unique songs. Interestingly, this means that ~86% of the songs on the Billboard Hot 100 were actually repeats from previous weeks.

Next, I downloaded as many of the lyrics of these songs as possible [[4]](TODO-link-to-script). Blah blah old charts before advent of the internet, lyrics not available, ended up with lyrics for N songs.

TODO scrape lyrics, find correlation coefficient between presence of "you" (and other words... love, me, I) and chart score. Chart score: each week, a song gets 100-n+1 points, where n is its position on the chart. That's one possible scheme, anyway. If there's no correlation, then David Rowley was talking shit. May want to control for the Beatles...is that ensured by repeating the test without Beatles songs?

### Conclusion
* Is it a valid tip?
* Mention that we've only shown a correlation. Experimental study required.
