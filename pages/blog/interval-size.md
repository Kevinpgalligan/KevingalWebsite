title: "Calculating the number of days until your birthday and why it's a useful programming trick"
date: 2019-10-26
draft: yes

It's December 8th. Your birthday is December 22nd. How many days are there until your birthday?

The answer, it probably won't interest you to find out, is: it depends. It depends on whether you include today in your count, and whether you include the day of the birthday itself. Something something 3 answers.

Something something recast the problem as a programming thing. If you're like me, you derped over this problem, writing out a small sample case to figure it out.

There's a simple formula that answers the question definitively, and it's super useful for programming and avoiding off-by-one errors. And also lots of real-life applications such as calculating the number of days until your birthday. (Bring in Aunt Catherine here, and then say f\*ck you Aunt Catherine at the end).

### Using the formula in an example
Consider the range 2-6.

   2 3 4 5 6

The "bounds" here are 2 and 6. If both of the bounds are *inclusive* (that is, they're included in the range, so the numbers in the range are 2, 3, 4, 5 and 6), then the number of elements in the range is 5, given by 6-2+1. More generally, that's U-L+1, where U is the upper bound and L is the lower bound.

     1+1+1+1+1=5
     v v v v v
    |2 3 4 5 6|

If one of the bounds is inclusive and the other is exclusive, then the number of elements is 4, given by U-L.

      1+1+1+1=4
      v v v v
    2|3 4 5 6|
    
     1+1+1+1=4
     v v v v
    |2 3 4 5|6

Finally, if both of the bounds are exclusive, then there are 3 elements in the range, given by U-L-1.

      1+1+1=3
      v v v
    2|3 4 5|6

To summarise, here are all general versions of the formula:

    Both bounds exclusive:        #E = U - L - 1
    One inclusive, one exclusive: #E = U - L
    Both bounds inclusive:        #E = U - L + 1
    (#E is number of elements in the range)

Let's relate it back to the birthday example from the start of the post. If you want to include today and the day of your birthday in your count, then there are 22-8+1=15 days until your birthday. Including just today .

### Conclusion
<!-- Fuck Shane! -->

You should now be able to count the number of days until your birthday with full confidence, and perhaps more usefully, avoid annoying off-by-1 errors in your programming. Happy counting!

