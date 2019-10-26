title: Did The Beatles really love "you"?
date: 2019-10-14
draft: yes

Browsing the [Wikipedia page of Lennon-McCartney](https://en.wikipedia.org/wiki/Lennon–McCartney), the virtuoso songwriting duo of Beatles fame, a fun fact caught my interest.

> Author David Rowley points out that at least half of all Lennon–McCartney lyrics have the words "you" and/or "your" in the first line.

This refers to page 3 of the book *Help! 50 Songwriting, Recording and Career Tips used by the Beatles*, which suggests that songwriters should put the word "you" in the first line of their lyrics:

> Around half of all Lennon-McCartney lyrics use the magic songwriting words 'you or 'your' in the first line.

> These words, especially when used with the words 'I' or 'me', allow the listener to imagine that the song is about them and allow the listener to imagine that they are singing the song to another person.

> The opening words of the following songs are all addressed directly to the listener. 'I Want to Hold Your Hand', 'We can Work It Out', 'Taxman', [...]. Indeed, on some of these songs it sounds as if the Beatles wrote the lyrics as though they were having a conversation with their listener.

This incredibly unquantified statement brings two incredibly important questions to mind.

* Exactly how many Beatles songs refer to "you" in the first line?
* Is there actually a correlation between chart success and the inclusion of "you" in the first line of a song, or is David Rowley blowing hot steam out of his ass?

This post will answer these questions.

![Photo of Lennon & McCartney]({{ url_for('static', filename='img/lennon-mccartney-and-you/beatles-eyeshot.png') }})

### How many Lennon-McCartney songs are addressed to "you"?
I wrote a [script](https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/experiments/beatles-and-you/beatles-lyrics-scraper.py) to download the lyrics from the Beatles' ["core catalogue" studio albums](https://en.wikipedia.org/wiki/The_Beatles_discography#Studio_albums). Instrumental songs, cover songs and songs credited to Ringo Starr / George Harrison were removed manually. This left a total of 140 songs. A bit short of the full ~180 songs published by Lennon-McCartney, but it's a representative sample.

After [RUNNING THE NUMBERS](https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/experiments/beatles-and-you/beatles-analysis.py), it was found that **30%** of the songs had "you" (or "your") in the first line. This is quite a bit short of "around half", but since **83%** of the songs contained "you" *somewhere* in the lyrics, I'm willing to give David Rowley a pass. It shouldn't be controversial to state that Lennon-McCartney did, in fact, love "you".

However, it would be a stretch to then issue a commandment like *goOd SoNgWriTeRs SAy yOu* in our bible of songwriting tips. Before that, the least we could do would be to establish a correlation between the use of the word "you" and success in music charts.

### Chart success and you
* Decide on the years + lyrics website (should be easy to go from artist's name on the chart, to a link for the lyrics website).
* Write script to scrape them.
* Manually nab the ones that fail.
* Once again do analysis.

### Conclusion
* Is it a valid tip?
* Mention that we've only shown a correlation. Experimental study required.
