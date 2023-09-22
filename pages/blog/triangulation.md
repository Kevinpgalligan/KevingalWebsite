title: "Triangulation pillars: an alien technology?"
date: 2023-09-21
description: A look at triangulation pillars and how they were used to create detailed maps before satellites.
publish: y
requires: math
tags: data ireland

In the image below is a [triangulation pillar](https://en.wikipedia.org/wiki/Triangulation_station), or triangulation station. Thousands of years ago, aliens placed this pillar at the top of Benbulbin mountain in Ireland. It contains advanced technology that allows the aliens to monitor human civilisation from across the galaxy.

<figure>
<img src="{{ url_for('static', filename='img/triangulation/benbulben.jpeg') }}"
     alt="A concrete pillar at the top of a mountain."
     class="centered">
<figcaption>The peak of Benbulbin, Ireland.</figcaption>
</figure>

Just kidding. It's a block of concrete that was placed there by humans in the past 200 years. This lump of dried cement was used to create maps of Ireland in the time before satellites. In this article we'll see how those maps were constructed, and we'll also see how badly the mapping could go wrong with even the slightest measurement error.

### How it works
To understand the idea behind triangulation pillars, let's see how they're distributed throughout Ireland. Here's a map of that distribution. The red triangle corresponds to the Benbulbin pillar shown above. I got the pillar coordinates from [here](http://www.trigpointing-ireland.org.uk/index.php), and the county boundary coordinates from [here](https://www.townlands.ie/page/download/).

<figure>
<img src="{{ url_for('static', filename='img/triangulation/map.jpg') }}"
     alt="A map of Ireland with a bunch of triangles marking the triangulation pillars."
     class="centered">
<figcaption>Ireland and its triangulation pillars.</figcaption>
</figure>

If we form triangles between all the pillars, we get a mesh that covers the entire country. I used `scipy.spatial.Delaunay` to do so.

<img src="{{ url_for('static', filename='img/triangulation/mesh-map.jpg') }}"
     alt="The same map but with red lines forming a mesh of triangles between all the pillars."
     class="centered">

Now, if we remove the country boundaries, it becomes clear that the triangle mesh formed by the pillars somewhat resembles the shape of Ireland as we know it today. 

<img src="{{ url_for('static', filename='img/triangulation/mesh-only-map.jpg') }}"
     alt="Map of Ireland, but the lines marking the borders have been removed. Only the triangulation pillar triangle mesh remains."
     class="centered">

This is roughly what a map of Ireland would've looked like in the 1800s when the first survey of Ireland was conducted using triangulation pillars, and it gives you an idea of how the pillars could be used to map the country. If you can figure out where each pillar is relative to its neighbours, then by joining them all together you have a pretty good idea of what the country looks like.

How does this work in practice? Once the pillars are in place, the first step in a hypothetical survey is to take a single triangle and measure one of its side lengths. In the Irish Ordnance Survey, beginning around 1827, the side length chosen was along the shores of Lough Foyle in Northern Ireland. The next step in the hypothetical survey is to measure the internal angles of each triangle using a device called a [theodolite](https://en.wikipedia.org/wiki/Theodolite). Basically, you stand at one of the pillars, which for visibility are built in high places, and put light sources at 2 of its neighbouring pillars. The theodolite can accurately determine the angle between the neighbouring lights, thus giving you an internal angle of the triangle formed by the 3 pillars.

Using these measurements -- a single side length and a bunch of angles -- a full map of the country can be reconstructed. The missing ingredient that ties it all together is the [law of sines](https://en.wikipedia.org/wiki/Law_of_sines), which you may have learned in school. Given a single side length and the internal angles of a triangle, the law of sines allows you to calculate the remaining 2 side lengths.

For example, suppose we're standing at Benbulbin, which is neighboured by triangulation pillars at Mullaghmore and Truskmore. Let's say we want to calculate the distance to Truskmore, labelled `x` in the diagram below, given that we know the angles between the 3 pillars and the straight-line distance from Benbulbin to Mullaghmore (here, 10km).

<img src="{{ url_for('static', filename='img/triangulation/law-of-sines.jpg') }}"
     alt="3 points: Benbulbin, Mullaghmore and Truskmore, forming a triangle."
     class="centered">

Using the law of sines,

```math
\frac{sin(65)}{x} = \frac{sin(45)}{10}.
```

Solving for `x` gives a distance of about 9.72km. Using this new distance estimate, we can estimate even more distances, which propagate through the triangle mesh until we know the relative positions of all the pillars in Ireland.

It's very important, however, that the initial distance measurement be accurate. Even the slightest error can propagate through the whole triangle mesh and mess up all the estimated distances.

### What can go wrong
Let's say we have accurately measured the angles between all the pillars on the map above. We don't know where the pillars are, though, or how far apart they are from each other. So the next step in a survey would be to measure the distance between a single pair of triangulation pillars. Let's say we measure the distance between Scalp Mountain and Creehennan Hill, both along the shores of Lough Foyle. Assuming the measurement has 0% error, that distance can be propagated through the whole triangle mesh using the law of sines, and the map of triangulation pillars can be reconstructed exactly.

But if there's even a small amount of error in the distance measurement -- say, 1% -- we get the following.



### Closing thoughts
I took that picture at Benbulbin myself, so I can appreciate how hard it must have been to capture all those measurements back in the day. There are 868 pillars in the dataset I looked at, and I shiver at the thought of climbing Benbulbin 868 times. A more detailed history of the 1800s survey can be found at [tripointing-ireland.org.uk](http://www.trigpointing-ireland.org.uk/trigpoints.php). 
