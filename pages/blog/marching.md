title: Linear interpolation in the marching squares algorithm
date: 2026-11-21
description: An explainer on how linear interpolation can be applied to the marching squares algorithm.
tags: graphics
requires: math
publish: y

Marching squares is an algorithm in computer graphics that takes a two-dimensional function and returns the approximate boundaries where the function value goes above a given threshold. That is, for a function `f(x,y)`, marching squares generates (approximately) the wobbly lines in 2d space where `f(x,y)=t` for some threshold `t`. (The technical term for these wobbly lines is ["contours"](https://en.wikipedia.org/wiki/Contour_line), apparently). Just to one side of the contours - assuming the function doesn't have any weird discontinuities - the function values will be above the threshold, and just on the other side they'll be below.

<figure>
<img src="{{ url_for('static', filename='img/marching/topographic-map.png') }}"
     alt="A random map with contour lines showing areas of the same height."
     class="centered">
<figcaption>Contour lines in a topographic map. Here, the function value in question would be height/elevation. (Picture source: Wikipedia page on contour lines).</figcaption>
</figure>

Anyway, the intention of this post isn't to explain marching squares in detail. For that, you can watch [The Coding Train's video](https://www.youtube.com/watch?v=0ZONMNUKTfU) on the subject, or maybe read [this blog post](https://jamie-wong.com/2014/08/19/metaballs-and-marching-squares/). Instead, we'll explore how linear interpolation can be applied to improve the results of marching squares. It's a small detail that has been glossed over in other explanations I've encountered, including the Coding Train's, and I had to spend a short time figuring it out in [my own implementation](https://github.com/Kevinpgalligan/sketches/blob/master/src/thesketches/marching.lisp). Now, dear reader, instead of working out for yourself this relatively simple piece of maths, you can spend the time being in communion with trees and dancing in corn fields.

<figure>
<img src="{{ url_for('static', filename='img/marching/no-interp.png') }}"
     alt="Random-looking black regions on a white background, with sharp edges."
     class="centered borderforwhitepic">
<figcaption>Marching squares applied to Perlin noise, without linear interpolation. The black regions show approximately where the function value is above the threshold.</figcaption>
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
     alt="A single cell (it's just a square). The top right corner has a red circle, and it separated from the others by a red line. The other corners have white circles with red borders."
     class="centered">
<figcaption>A single cell from the grid. The top-right corner has a value above the threshold, and the approximate contour is given by drawing a line to separate that corner from the others (which have values below the threshold).</figcaption>
</figure>

Where do we draw the separating lines? In the basic version of marching squares, as demonstrated by the ever-wonderful Coding Train, the lines are drawn from halfway along the edges of the cell. We're gambling that the contour, i.e. the point where `f(x,y)=t`, will be halfway between the point that's below the threshold (call it $`P_1`$; $`f(P_1)<t`$) and the point that's above the threshold ($`P_2`$; $`f(P_2)>t`$).

<figure>
<img src="{{ url_for('static', filename='img/marching/edge-halfway.png') }}"
     alt="A single edge of the cell; a horizontal line. Left endpoint is labelled P1, value of 0.1. Right endpoint is labelled P2, with a value of 5.0. A red line, representing our approximation of the contour, is drawn from halfway along the edge."
     class="centered">
<figcaption>The top edge of the cell, with function values of 0.1 and 5.0 for P1 and P2. In the basic version of the algorithm, we assume the contour passes through the halfway point of the edge.</figcaption>
</figure>

But what if $`f(P_1)`$ is way below the threshold, and $`f(P_2)`$ is just a small bit above the threshold? In the example we just saw, let's say the threshold is $`t=4.0`$. Then, assuming the function has somewhat linear behaviour, we'd expect the contour to be closer to $`P_2`$.

<figure>
<img src="{{ url_for('static', filename='img/marching/edge.png') }}"
     alt="A single edge of the cell. A horizontal line. Left endpoint is labelled P1, value of 0.1. Right endpoint is labelled P2, with a value of 5.0. And a point near the right endpoint is also marked, with a value of 4.0."
     class="centered">
<figcaption>Showing the point, labelled "C", that lies exactly on the contour, with a function value exactly equal to the threshold (4.0).</figcaption>
</figure>

This is where linear interpolation comes in. We use it to get a better estimate of the position of $`C`$. We assume that the function is linear along the line from $`P_1`$ to $`P_2`$. In other words, if you go a third of the way from $`P_1`$ to $`P_2`$, then the value of the function will be a third of the way from $`f(P_1)`$ to $`f(P_2)`$. More generally, if you take a point that's a fraction $`d`$ along the line from $`P_1`$ to $`P_2`$, then the function value should be the same fraction $`d`$ from $`f(P_1)`$ to $`f(P_2)`$. We can express express the fraction as

```math
d = \frac{t - f(P_1)}{f(P_2) - f(P_1)}.
```

Then, the estimated contour position is given by

```math
C \approx P_1 + d(P_2 - P_1).
```
