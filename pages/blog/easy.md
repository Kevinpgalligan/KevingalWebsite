title: An easy answer is hard to find
date: 2021-05-10
description: Being single-minded can get you trapped in a hole.
imgthumbnail: img/easy/thumbnail.jpg
publish: y
tags: problem-solving

After a couple of weeks in Twitter's spam dragnet, [TokiPonaPoemBot](https://twitter.com/PonaBot) is now up and running! It's based on the [poem generator](/blog/toki-poetry.html) I wrote about in March. Twitter could have done a better job of letting me know why the account was blacklisted, and there was no indication that they would ever reply to my support request, but alas -- no billion-dollar megacorp is perfect. It probably got marked as spam because I registered with my university email address.

ANYWAY, what I actually wanted to write about today was an experience I had while developing TokiPonaPoemBot. In that experience, there's a lesson about being pragmatic versus being a single-minded dumbdumb, which I think is worth sharing.

Here's the background. Initially, I intended to run the bot as a systemd service, systemd being Linux software that can manage processes. My idea was that systemd would wake up the bot whenever I booted my computer. The bot would then ask itself "Is it time to tweet yet?" in a loop until it was time to tweet. The thing is, it wasn't working. The bot's systemd service always failed to start, and I couldn't figure out why. It seemed likely that my motivation would die and I would never finish the project.

Then a simpler solution occurred to me. There's another piece of Linux software called cron that runs tasks at regular intervals. With the following configuration, I could get cron to run the bot once per day at 4pm:

    0 16 * * * cd /path/to/bot/ && ./tweet.lisp >>tweet.log

Easy as pie! And by letting cron handle the schedule, I was able to rip out the part of the bot that asked "Is it time to tweet yet?", which made the code much cleaner.

There are pros and crons to both approaches. There might not be an equivalent of cron for Windows, for example, if I ever want to run the bot there. But cron was, unquestionably, the better and more pragmatic solution, because it got the job done and saved me from further frustration. It's good enough for now, and possibly forever.

My question for you is this: why was I fixated on doing it the systemd way, and why didn't I come up with the cron solution earlier? My brain latched on to the first design I came up with, and I didn't step back to look for a lazier (better) solution until I became super frustrated.

It's a pattern I've noticed. You get trapped in a brain hole and forget the context of what you're doing: "this needs to be a systemd service", "this function needs to be optimised", "this algorithm needs to be multi-threaded", "my application needs this feature".

In each of those cases, there's a question you should ask that could save you a lot of trouble. Is there a more lightweight alternative to systemd? Is that function going to be called often enough to make optimisation worthwhile? Could memory be the actual bottleneck of your algorithm, making multi-threading redundant? Would anyone actually use that feature if you added it to your application?

So besides developing our problem-solving skills, we must strive to be pragmatic, to avoid brain holes, and to always keep in mind the wider context of what we're doing. Otherwise, we'll end up wasting our time. Even after nearly 10 years of programming, I still often make this mistake.
