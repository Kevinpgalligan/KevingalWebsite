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
* Is there actually a correlation between chart success and the inclusion of "you" in the first line of a song, or is David Rowley blowing hot steam out of his orifices?

This post will answer these questions.

![Photo of Lennon & McCartney]({{ url_for('static', filename='img/lennon-mccartney-and-you/beatles-eyeshot.png') }})

### How many Lennon-McCartney songs are addressed to "you"?
I wrote a [script](https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/experiments/beatles-and-you/beatles-lyrics-scraper.py) to download the lyrics from the Beatles' ["core catalogue" studio albums](https://en.wikipedia.org/wiki/The_Beatles_discography#Studio_albums). Instrumental songs, cover songs and songs credited to Ringo Starr / George Harrison were removed manually. This left a total of 140 songs. A bit short of the full ~180 songs published by Lennon-McCartney, but it should be a representative sample.

After [RUNNING THE NUMBERS](https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/experiments/beatles-and-you/beatles-analysis.py), it was found that **30%** of the songs had "you" (or "your") in the first line. This is quite a bit short of "around half", but since **83%** of the songs contained "you" *somewhere* in the lyrics, I'm willing to give David Rowley a pass. It shouldn't be controversial to state that Lennon-McCartney did, in fact, love "you".

However, it would be a stretch to then issue a commandment like *goOd SoNgWriTeRs SAy yOu* in a bible of songwriting tips. Before that, the least we could do would be to establish a correlation between the use of the word "you" and success in music charts.

### Chart success and you
We're going to try to find a correlation between success in music charts and "you". We consider chart success to be the highest position reached on the charts, so a song that peaks at #1 is more successful than one that peaks #5. The chart we'll be looking at is Billboard Top 100, which has been running since 1956.

> The Billboard Hot 100 is the music industry standard record chart in the United States for songs, published weekly by Billboard magazine. Chart rankings are based on sales (physical and digital), radio play, and online streaming in the United States. [source](https://en.wikipedia.org/wiki/Billboard_Hot_100)

The Billboard Hot 100 charts were scraped (TODO should "scraped" be used?) (TODO link to script) from the start of 1962 until the start of 1970. That's 417 weeks; a total of 5681 unique songs reached the charts during that time. Interestingly, this means that ~86% of songs listed on the charts were actually duplicates (TODO wording?) from previous weeks.

Remaining stuff to do:

* Scrape lyrics for those songs, as much as possible.
* Find correlation coefficient between presence of "you" and chart peak.

### Conclusion
* Is it a valid tip?
* Mention that we've only shown a correlation. Experimental study required.
