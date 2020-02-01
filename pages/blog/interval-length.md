title: "Counting the number of days until your birthday and why it's a useful programming trick"
date: 2020-02-01

Spot quiz! See how many of these calculations you get right. No calculator necessary.

1. Today is March 2nd, your birthday is March 28th. How many days remain until your birthday, not including today and not including your birthday?
2. You have an array with 100 elements. How many elements are there between index 3 and index 97, not including the element at index 3 and including the element at index 97?
3. If you have 10 cabbages in a row, how many cabbages are there between the 2nd cabbage and the 9th cabbage, including the 2nd cabbage but not the 9th one?
4. A group of citizens in the Dystopian city of Fencepostia, with ID numbers from 50 to 60, have been selected for thought purging. How many citizens have been selected, including both citizen #50 and citizen #60?

Answers in 5...

4...

3...

2...

1...

<small>spoiler: 25, 94, 7 and 11.</small>

If you got all of them correct without finger-counting, without drops of nervous sweat rolling down your neck, and without asking any questions on StackOverflow, then congratulations. You are an Übermensch of calculation, and there is nothing for you to learn here.

If not, then you've fallen victim to one of the 2 hard problems\* in computer science: the off-by-1 error.

Off-by-1 errors have been written about since before Jesus Christ. At the latest, they were mentioned by [a Roman named Vitruvius](https://web.archive.org/web/20160305221341/http://www.dsm.fordham.edu/~moniot/Opinions/fencepost-error-history.shtml), who served in the army of Julius Caesar. He was talking about the number of fenceposts around a temple, but the principle is the same. The problem is old and wily, and you shouldn't feel bad if it caught you out.

The good news is that there's a simple, easy-to-remember formula that applies to all of the above calculations. Once you've learned it, you'll never again suffer from this particular brand of off-by-1 horror. Nor will you miscount the number of days until Aunt Catherine's birthday.

<img src="{{ url_for('static', filename='img/interval-length/aunt-catherine.jpg') }}"
     alt="Angry Aunt Catherine."
     class="centered">

### Give me the formula
Let's reconsider all of the above problems as calculations of interval length.

What's an interval? It's a set of integers that lies between a lower and upper bound. For example, the interval `[3, 7]` contains the numbers 3, 4, 5, 6 and 7. It's a set, so we write it using the set notation `{3,4,5,6,7}`.

The bounds of an interval can be *open* or *closed*. If a bound is open, it means that the bound itself is excluded from the interval. If a bound is closed, it's included. In the above example, both bounds were closed. The same interval with open bounds, written `(3, 7)`, is the set `{4,5,6}`.

The bounds can also be half-open. The half-open intervals `(3, 7]` and `[3, 7)` are `{4,5,6,7}` and `{3,4,5,6}`, respectively.

Now, let's rephrase the problems from before in terms of intervals.

* Birthday: the open interval `(2, 28)`, contains 25=28-2-1 elements.
* Array: the half-open interval `(3, 97]`, contains 94=97-3 elements.
* Cabbages: the half-open interval `[2, 9)`, contains 7=9-2 elements.
* Dystopia: the closed interval `[50, 60]`, contains 11=60-50+1 elements.

We have all 3 types of interval here: open, half-open and closed. As such, we can generalise the little calculations we did so that they work for any interval. If you have an open interval `(L, U)`, then the number of elements is `U-L-1`. If you have a half-open interval `(L, U]` or `[L, U)`, then it's `U-L`. And finally, if you have a closed interval `[L, U]`, it's `U-L+1`. They're all the same formula, you just have to add 1 if both bounds are closed and subtract 1 if they're both open.

This is the reason why programming interfaces use half-open bounds for ranges. In Python, for example, you say `mylist[L:L+N]` to copy the list items with indexes in the interval `[L, L+N)`. That's (L+N)-L=N elements. Much neater than having rogue +1s and -1s floating around the place.

That's it. I was going to make this into a whole big thing, but it's really that simple. 

<!-- Fuck Shane! -->

### Farewell to Fencepostia
You should now be able to count the number of days until your birthday with full confidence, and perhaps more usefully, avoid annoying off-by-1 errors in your programming. I used to come up with small example cases of this problem and finger-count in order to figure it out. I didn't realise that there was a better way, and I never had it explained to me. If you search the internet for "subarray length", you'll come across scary topics like "finding the length of the largest subarray with contiguous elements". It doesn't seem to be written down anywhere. Maybe that's because it's obvious, but it wasn't for me.

On that note, happy counting, and say hello to Aunt Catherine.

<small>\*From the over-used joke: "There are 2 hard problems in computer science: cache invalidation, naming things, and off-by-1 errors."</small>
