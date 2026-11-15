title: Linear interpolation in the marching squares algorithm
date: 2026-11-15
description: Explaining how linear interpolation works in the marching squares algorithm from the world of 2d graphics.
tags: programming math

Marching squares is an algorithm from computer graphics that takes a two-dimensional function and returns the approximate boundaries where the function value goes above a given threshold. That is, for a function `f(x,y)`, marching squares generates (approximately) the lines in 2d space where `f(x,y)=t` for some threshold `t`. (These lines are apparently called [contours](https://en.wikipedia.org/wiki/Contour_line)). Just to one side of the contours - assuming the function doesn't have any weird discontinuities - the function values will be above the threshold, and on the other side they'll be below.

<figure>
<img src="{{ url_for('static', filename='img/marching/topographic-map.png') }}"
     alt="A random map with contour lines showing areas of the same height."
     class="centered">
<figcaption>Contour lines in a topographic map. Here, the function in question would be height/elevation. (Picture source: Wikipedia).</figcaption>
</figure>

Anyway, this post isn't about explaining marching squares. For that, you can watch the [Coding Train's video](https://www.youtube.com/watch?v=0ZONMNUKTfU) on the subject. Instead, we'll explore how linear interpolation can be applied in marching squares to smooth out the results. It's a small detail that was glossed over in other explanations I encountered, including the Coding Train's, and I had to spend a short time figuring it out in [my own implementation](https://github.com/Kevinpgalligan/sketches/blob/master/src/thesketches/marching.lisp) of marching squares. Now, dear reader, instead of working out for yourself this relatively simple piece of maths, you can spend the time contemplating the meaning of our existence.

<figure>
<img src="{{ url_for('static', filename='img/marching/no-interp.png') }}"
     alt="Random-looking black regions on a white background, with sharp edges."
     class="centered borderforwhitepic">
<figcaption>The result of marching squares applied to Perlin noise, without linear interpolation.</figcaption>
</figure>

<figure>
<img src="{{ url_for('static', filename='img/marching/interp.png') }}"
     alt="Same as the last but with smoother edges"
     class="centered borderforwhitepic">
<figcaption>The same result, but with linear interpolation. It's clearly a closer approximation of the contours / boundaries, because Perlin noise is supposed to be smooth.</figcaption>
</figure>

So, what does Mr. Coding Train mean when he says "just use linear interpolation"? If you'll recall, the marching squares algorithm divides the target region into a grid of cells. For each cell, the 2d function in question is evaluated at the corners of the cell. Some corners will be found to have values above the threshold, some will be below. Then, lines are drawn to separate the corners above the threshold from the ones below the threshold. These lines are approximations of the contours.

<figure>
<img src="{{ url_for('static', filename='img/marching/cell.png') }}"
     alt="TODO"
     class="centered">
<figcaption>TODO</figcaption>
</figure>

Next:

* Explain the pic.
* Isolate the top edge of the cell, label the different parts.
* Explain how, at some point along that edge, we expect f(x,y)=t, which lies exactly on the real contour.
* But marching squares is an approximation, we don't want to exhaustively evaluate the function everywhere to find the contours.
* Basic approach is to just draw halfway between the two corners. That's what's shown in the Coding Train video.
* But in reality, corner A might be way above the threshold. And corner B might be very close to the threshold. In which case we'd expect the contour (where the function equals the threshold) to be closer to corner B.
* So, instead we assume the function value increases linearly from point B to point A, and calculate where it would cross the threshold.
* The math: d = (t - f(B))/(f(A) - f(B)), and Q = B + d(A - B).
* Q is the point on the contour; B is point below threshold, A is above.
