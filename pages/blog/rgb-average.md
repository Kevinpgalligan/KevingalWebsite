title: Generating random RGB tuples with a target average value
date: 2019-10-14
draft: yes

Today we'll look at a problem with zero practical applications: generating a random RGB tuple where the average of R, G and B is a given integer N in the range \[0, 255\].

Given a whole number N in the range 0-255, our goal is to generate a random RGB tuple (R, G, B) with R, G, B integer and 0 <= R, G, B <= 255 such that the average of R, G and B is N, i.e. (R + G + B) / 3 = N.

[Source of problem.](https://www.reddit.com/r/computerscience/comments/a1ea1s/create_program_to_generate_random_rgb_values_that/)

### First shot
We have to find R, G, B such that (R + G + B) / 3 = K.

Multiplying across, we have R + G + B = 3K.

R, G and B can take any integer value in the range [0, 3K].

We can generate them one at a time.

First, set `remaining := 3K`.

Then set `R := randint(max(remaining - 3*255, 0), min(remaining, 255))`.

Then set `G := randint(max(remaining - 1*255, 0), min(remaining, 255))`.

Then set `B := randint(max(remaining - 0*255, 0), min(remaining, 255))`.

Blah blah this seems okay, but ackshually... (show graph with distribution of values for R, G and B; should be uneven).

### FIX IT NOW
Generate (i1, i2, i3) as above, shuffle to give (R, G, B).

### Is there a way to do it without the shuffle?
I dunno.

### Generalisation
Generate tuple V = (V1, V2, ..., Vn) such that average(V) = K, where Bl <= Vi, K <= Bu.

Should be easy nuff.
