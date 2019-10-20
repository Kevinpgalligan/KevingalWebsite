title: The Tragic Story of u/AnEmojipastaBot
date: 2019-09-29

It 🤔 was a ❤ dark 🕶🔫 and 🌰 stormy night... 🌜

u/AnEmojipastaBot was a Reddit bot inspired by the very memey subreddit <a href="https://www.reddit.com/r/emojipasta">r/emojipasta</a>. The idea was simple: turn the comments of other users into emojipasta, on request. An example:

![An example of u/AnEmojipastaBot in action]({{ url_for('static', filename='img/emojipasta/firstexample.png') }})

The bot was a surprise hit, spreading across the entire website and acquiring 30k karma (or "upvotes") in a couple of days and keeping me busy with excitement and bug fixes. Eventually, it made too many enemies and its account was banned permanently by the admins of Reddit, supposedly for being spammy and annoying.

It was fun while it lasted, though, and rather than let the code (which generates rather good emojipasta) go to waste, I recently resurrected it <a href="{{ url_for('specific_app', name='emojipasta') }}">as a web app</a>.

In the rest of the post I'll give an overview of how the emojipasta generator works, some of the bot's greatest hits from Reddit, and mention some of the bugs I encountered during the few days that it was live.

### How It Works
I wrote a script to download a few thousand comments off r/emojipasta, keeping track of which words appeared beside which emojis. The bot broke up comments into chunks of text and emojis were inserted randomly between the chunks using the word-\>emoji "mappings" from r/emojipasta, with a maximum of 2 emojis after each chunk. Only the alphanumeric prefix of each chunk of text was considered, so that "I'll" would give the same emoji mappings as "I".

That's about it. The code was pretty simple. The original source code of the bot, in Python, can be found <a href="https://github.com/Kevinpgalligan/EmojipastaBot">HERE</a>. The JavaScript version that I ported recently can be found <a href="https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/static/js/apps/emojipasta/emojipasta.js">HERE</a>.

### Hall of Fame
The bot's top comments during its brief lifespan.

* **Oh. This was a bit dark. It was in a post about a news website inappropriately using emojis in its report of a school shooting.**
![An example of u/AnEmojipastaBot in action]({{ url_for('static', filename='img/emojipasta/secondexample.png') }})

* **Also a tad dark.**
![An example of u/AnEmojipastaBot in action]({{ url_for('static', filename='img/emojipasta/thirdexample.png') }})

* **More innocent.**
![An example of u/AnEmojipastaBot in action]({{ url_for('static', filename='img/emojipasta/fourthexample.png') }})

* **Cop story.**
![An example of u/AnEmojipastaBot in action]({{ url_for('static', filename='img/emojipasta/fifthexample.png') }})

* **Dooby brothers.**
![An example of u/AnEmojipastaBot in action]({{ url_for('static', filename='img/emojipasta/sixthexample.png') }})

* **Physical and mental pain.**
![An example of u/AnEmojipastaBot in action]({{ url_for('static', filename='img/emojipasta/seventhexample.png') }})

### Bug Trivia
I had to fix quite a few bugs in the bot's code as it picked up momentum on Reddit. It might be useful for future aspiring bot creators to be aware of them. Here is a possibly-non-comprehensive list:

* Infinite looping with other bots.
* Bot didn't respond when its username wasn't capitalised (e.g. "u/anemojipastabot" instead of "u/AnEmojipastaBot").
* Posting in inappropriate subreddits, e.g. r/suicidewatch.
* Crash when original comment had been deleted.
* Crash when posting in a subreddit where it had been banned.
* Didn't handle random network-related exceptions when calling Reddit API.

### The end
EmojipastaBot can now be laid to rest. Goodnight, sweet prince ⚰️
