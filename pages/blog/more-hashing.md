title: More on hash collisions
date: 2025-11-26
description: Replying to some comments on my hash collision article.
requires: math
tags: probability meta
publish: y

I was doing a sweep of Hacker News to see if any of my blog posts had been doing the rounds, and came across a [thread](https://news.ycombinator.com/item?id=44343293) concerning my [hash collision post](/blog/collisions.html). The thread is closed to further comments, so I'm sharing my responses here instead.

### On the approximation proof being wrong
One user writes:

> In "Appendix A: Supporting proofs for approximations" the author justifies the approximation of *1-x* with *e^(-x)* by computing *lim_{x→0} (1-x)/e^(-x) = 1*.
>
> This is wrong. Consider how this criteria is also satisfied by *1-100000x*, since *lim_{x→0} (1-x)/(1-100000x) = 1*. But this is clearly not a good first-order approximation for *1-x* around 0.
>
> The proper justification for replacing *1-x* with *e^-x* around 0 is done by examining the first 2 terms of their Taylor expansions, in other words, the functions' value at 0 and their first derivative at 0. Since these match for *1-x* and *e^-x*, they are good first-order approximations of each other around 0.

Fair point, though I would argue that a similar refutation can be found for the alternative criterion suggested here. For example, consider two functions $`f(x)=x^{2}`$ and $`g(x)=x^{100000}`$. They satisfy $`f(0)=g(0)`$ and $`f'(0)=g'(0)`$, but for practical purposes they're terrible approximations of each other.

Perhaps it would be more accurate to say that these criteria are both *necessary* but not *sufficient* for a good approximation? I still haven't found a solid definition of what makes a "good" approximation, probably because the bar for "good" is arbitrary. I'm sure, though, that there are methods in analysis for comparing how quickly functions converge to each other. This is the part where a mathematician steps in and tells us what's what.

I'm reminded of a similar phenomenon in the analysis of algorithms. According to Big-O notation, an $`O(n)`$ algorithm (whose complexity grows linearly) should require less steps than an $`O(n^2)`$ algorithm (whose complexity grows quadratically) as $`n`$ becomes arbitrarily large. However, for all practical values of $`n`$, such as the number of atoms in the Universe, we might still find that the $`O(n^2)`$ algorithm is more efficient. All that to say: yes, asymptotic arguments don't always hold up in reality.

### Some cool maths stuff
Someone wrote an interesting comment on using Stirling's approximation:

> The quickest way to work around the numeric overflow issues is to use the Stirling approximation of the logarithm of the factorial,
> 
> [link](https://en.wikipedia.org/wiki/Stirling's_approximation#Speed_of_convergence_and_error_estimates)
> 
> You can build on that to build good, composable approximations of any the standard combinatorial functions, in log-space (and recover the approximations you want by simply exponentiating back). For example, if you've implemented an approximate ln-fac(n) ~ ln(fac(n)), you immediately get ln-choose, the logarithm of (n!)/(k!(n-k)!), as simply ln-fac(n) - ln-fac(k) - ln-fac(n-k). Fully composable: if ln-fac() is a numerically good approximation, then is so any reasonable sum or difference.
> 
> Or: the log of the binomial distribution PDF is simply ln-choose(n,k) + k\*ln(p) + (n-k)\*ln(1-p).

Another user suggested alternative derivations for the final approximation:

> There's several ways to arrive at the same bound besides the explicit birthday paradox calculation:
> 
> * nC2 pairs of objects, each of which has a 1/k chance of matching. Since expectation is linear and it's also known that E[x] = p(x>=1) + p(x>=2) + ..., we have that p(x>=1) = E[x] - epsilon (since we can assume probability of two collisions and above is small) and so probability of collision is ~n^2 / 2k.
> 
> * Alternatively you can use union bound to see that probability of at least one match is bounded-above by sum of probabilities of any single event, so is <= n^2 / 2k, with goodness of approximation given by fact that probability of multiple events occurring simultaneously is low.
> 
> (The two are effectively the same proof, as indicator random variables can be used to show P(union of indicators) <= E[X], which is effectively a special case of markov inequality)

To both these users, I say... cool!

### On why I do this shit
A fellow soul-searcher asked...

> Honest question.
> 
> How does one write something like this?
> 
> I get the interest, and the review process. What I mean is, is this a hobby where someone is passionate about soothing, or does some employers allow people to work on side projects?
> 
> I feel my life is mostly about direct value, and I don't really understand where I went wrong in the path for meaningful knowledge.
> 
> Any philosophical help will be very welcome, as you correctly guest I'm a bit lost.

There's a line from my old diary, written in my early 20s, where I reflected on how I hadn't created or done anything of value in my life. I'd learned how to program computers, I'd always enjoyed writing, and I could play the guitar... but I hadn't *done* anything with those skills. Eventually, I started trying to change that. My blog was born, and I turned some of my projects into software that could actually be useful to people. To this day, I continue developing free software and writing blog posts as a form of creating meaning in my life. (No progress on the music front, though).

In starting a blog, I was inspired by the programming community's blogging culture. I'd learned a lot from programming blogs over the years and wanted to give back to that ecosystem. *Someone* has to write an explainer post on the minutiae of [symbols in Common Lisp](/blog/cl-symbols.html).

There's certainly an element of validation-seeking in my writing and in sharing what I write. Whenever I post something on social media, whether that's sharing one of my blog posts on Reddit or dropping a message in a Discord chat, I find it hard not to check back regularly to see how I've been received. A positive reception can be thrilling, as I bask in the warm glow of other people's approval. If the reception is negative or indifferent, then I walk away with a bleeding ego. (Yes, I should probably just stop using social media altogether). It's hard to tell how much this validation-seeking is driving my behaviour. I *think* I would still write if there were nobody besides myself to read what I wrote, but the form and content would probably be different.

I remember being fascinated by a post on Richard Stallman's website in which he casually mentioned that he was driven by the desire for appreciation from fellow programmers. "You can just say that?" I thought, "Reveal your very human weakness and your craving for approval? And nobody will laugh at you?" Given past events, Stallman obviously isn't a model of correct or ethical behaviour, but I enjoy when people are viscerally honest like this, whether it comes from bravery or shamelessness or obliviousness. 

Another of my motivations for creating things and releasing them into the world is that it's an opportunity to connect with others. I've received emails from people I never would've connected with otherwise, which thus far has always been a pleasant and enriching experience. Ya never know what opportunities might arise from that.

Not every programmer needs to have a blog or an open source project to work on. Some people hate writing, or prefer not to program outside their job - and that's okay. This just happens to be one of the ways I find meaning and fulfillment in life, and in my opinion it has created more value for society than anything I've done as part of a paid job, minuscule as that value may be.

### On praise and encouragement
Someone left a kind and complimentary comment that I was delighted to read. There's the sweet, sweet validation I was talking about!

> The author uses writing techniques like those given by Joel Spolsky:
> 
> - Rule 1: Be Funny
> - Rule 2: Writing a spec is like writing code for a brain to execute
> - Rule 3: Write as simply as possible
> - Rule 4: Review and reread several times
> 
> The author isn't quite as adept at integrating the humor as seemlessly as Joel, yet it's interesting to see how effective the style is, even for someone still learning it. I commend them for making the topic more accessible. It was probably fun to write and was definitely fun to read!

I don't often get positive feedback on my writing. There's always someone ready with an "um, ackshually" to dismiss the work of others. The top comment under my very first blog post, which was about simulating boarding times for planes, basically said that the whole thing was pointless, because other parts of flight preparation take longer[^boarding]. A Hacker News commenter once said I was "lacking in many respects" because I made fun of Snakes & Ladders. And in response to my [post](/blog/java-overloading.html) about Java and operator overloading, in which I mimicked Guy Steele's conceit of speaking in 1-syllable words, I was lambasted for not properly following the rules of the conceit. In replying to such comments, I find it hard not to fall into the trap of sounding salty, or, in over-correcting for that, sounding like a pathetic sycophant. So, it's probably better to ignore them and move on.

Anyway, it won't surprise you to hear that I prefer positive or constructive feedback. It encourages me to keep going, and I try to pass on that encouragement to others when I enjoy what they write. A big THANK YOU to this user, although I would like to add that I am WAY funnier than Joel Spolsky.

[^boarding]: Okay, Mr. Smartypants, but if you watched that CPG Gray video you referenced, you'd know that he also mentioned an airline that brags about their short boarding times, so clearly it IS important to airlines! (This is the accumulated bitterness of 6 years).
