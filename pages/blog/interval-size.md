title: "Programming trick: interval size formula"
date: 2019-10-08

Alternative title: Calculate Interval Size With This 1 Weird Trick! Mathematicians HATE It!

When you're given 2 index values of an array and need to calculate the number of elements between them, it's surprisingly easy to mess up and get an off-by-1 error, especially when you start changing whether the bounds are inclusive / exclusive.

As it turns out, there's a simple formula for calculating interval size, and we're going to cover it in this article so that you're never again troubled by off-by-one errors when calculating the number of days until your birthday.

### The problem
We're asked to calculate the number of elements in this list between index 3 and index 7.

    [a b c d e f g h i j]
     0 1 2 3 4 5 6 7 8 9

Obviously, the answer is `7 - 3 = 4`, right?

Not really. It depends on whether we consider the "bounds", 3 and 7, to be inclusive or exclusive. If a bound is inclusive, it means we include the element at that index in our calculation. If it's exclusive, we exclude the element.

We can see that, if both 3 and 7 are exclusive, there are 3 elements in the specified interval.

             v v v
    [a b c d|e f g|h i j]
     0 1 2 3|4 5 6|7 8 9

If 3 is inclusive and 7 is exclusive, or 3 is exclusive and 7 is inclusive, there are 4 elements.

           v v v v
    [a b c|d e f g|h i j]
     0 1 2|3 4 5 6|7 8 9

             v v v v
    [a b c d|e f g h|i j]
     0 1 2 3|4 5 6 7|8 9

Finally, if both 3 and 7 are inclusive, there are 5 elements.

           v v v v v
    [a b c|d e f g h|i j]
     0 1 2|3 4 5 6 7|8 9

If we look at the example closely and think hard for a moment, out pops...

### The magic formula

    Open:      #E = U - L - 1
    Half-open: #E = U - L
    Closed:    #E = U - L + 1

### Conclusion
<!-- Fuck Shane! -->

You should now be able to count the number of days until your birthday with full confidence, and perhaps more usefully, avoid annoying off-by-1 errors in your programming. Happy counting!

