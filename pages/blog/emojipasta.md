title: The story of u/AnEmojipastaBot
description: A Reddit bot whose function was to turn comments into emojipasta.
date: 2019-09-29

u/AnEmojipastaBot was a Reddit bot inspired by <a href="https://www.reddit.com/r/emojipasta">r/emojipasta</a>. The idea was simple: turn the comments of other users into emojipasta, on request.

For example...

> u/JohnDoe1: it's getting hot in here

> u/JohnDoe2: <span style="color: blue">u/anemojipastabot</span>

> u/AnEmojipastaBot: it's getting hot 🔥😍 in 🔽👏 here 💪👏

The bot was a surprise hit, spreading across the entire website and acquiring 30k upvotes (or "karma") in a handful of days. Reddit users and other bots broke it quickly in a number of surprising and amusing ways:

* The bot didn't respond when its username was uncapitalised ("u/anemojipastabot" instead of "u/AnEmojipastaBot").
* People got it to post in inappropriate subreddits, like r/suicidewatch.
* It entered infinite loops with other bots. The other bot would mention AnEmojipastaBot, then AnEmojipastaBot would mention the other bot, then the other bot would mention AnEmojipastaBot, and so on.
* It crashed if the target comment got deleted.
* It crashed if it was summoned in a subreddit where it had been banned.
* It didn't handle random network-related errors when calling the Reddit API.

These bugs were squashed, however, and everything began to run smoothly. Here are the most popular comments of the bot from those carefree days.

* [🔫🔫🔫Shooting🔫🔫🔫At 🔫 Maryland🇺🇸High⬆️School📚🏫🎓](https://www.reddit.com/r/FellowKids/comments/85ty5v/ah_yes_thats_an_appropriate_use_of_emojis/dw0aiam/)
* [Those limps are 🗑 called depression, anxiety, PTSD, and 💰😏 addiction. 💊](https://www.reddit.com/r/Showerthoughts/comments/863ag0/if_getting_hurt_physically_can_leave_you_walking/dw259jx/)
* [Greetings travelers! Welcome to my profile!](https://www.reddit.com/r/copypasta/comments/85iurk/welcome_to_my_steam_profile/dvxqg19/)
* [I 👀 pretended to 💦👏 be a girl 🤰💃 in 🌤 the 🎁 new roblox 😮 server 🙅🏻](https://www.reddit.com/r/copypasta/comments/85658e/ultimate_roblox_scam/dvv41mm/)
* [shitty twitter memes ♀💩](https://www.reddit.com/r/me_irl/comments/85zd42/me_irl/dw1i2qo/)
* [💂 anything illegal 😶 in 😍🔙 the car 🍳 👮](https://www.reddit.com/r/BlackPeopleTwitter/comments/86xtyf/what_happened_to_click_it_or_ticket/dw8t9bx/)
* [Dooby brothers! 👬👬](https://www.reddit.com/r/starterpacks/comments/867exf/that_one_girl_who_is_obsessed_with_being_positive/dw39nl9/)
* [physical 💏 and mental ⚖ pain 😧](https://www.reddit.com/r/HistoryMemes/comments/85zmhw/oc_as_a_history_buff_does_this_trigger_you_as/dw1flmj/)

Eventually, the bot made enemies with too many important people and its account was banned permanently. For being spammy and annoying, apparently. Nobody could have seen it coming.

It was fun while it lasted, however, and rather than let the code go to waste, I recently resurrected it <a href="{{ url_for('specific_app', name='emojipasta') }}">as a web app</a>.

For the interested, here's a brief description of how it worked (and how it still works in the web app). I wrote a script to download a few thousand comments off r/emojipasta. These were used to create "mappings" from words to the emojis that tend to appear beside them. When it was summoned, the bot broke up comments into chunks of text and emojis were inserted randomly between the chunks using the word-\>emoji mappings, with a maximum of 2 emojis after each chunk. Only the alphanumeric prefix of each chunk of text was considered, so that "I'll" would give the same emoji mappings as "I".

That's about it. The code was pretty simple. The original source code of the bot, in Python, can be found <a href="https://github.com/Kevinpgalligan/EmojipastaBot">here</a>. The JavaScript version that I ported recently can be found <a href="https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/static/js/apps/emojipasta/emojipasta.js">here</a>.
