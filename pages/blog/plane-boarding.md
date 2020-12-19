title: Simulating plane designs for faster boarding
date: 2019-09-07
description: Moving entrances closer to the middle could make plane boarding faster.
publish: y

Boarding a plane, you wait behind a man so unwashed that you can almost see a stink cloud emanating from his trenchcoat. Two rows ahead, an old woman struggles to lift her humongous suitcases, each of which is large enough to fit her inside. Behind you is a red-eyed, defeated mother, holding a shrill baby. "There has to be a faster way to get through this," you think to yourself. Then the baby vomits into your backpack.

### A faster way
Consider a typical plane in Europe, such as Ryanair's Boeing 737-800. 2 entrances for passengers, one at each end of the plane. 200 seats spread across 32 rows, with an aisle down the middle.

Passengers board using the entrance that is closest to their seat. This makes boarding more efficient because passengers from the back and passengers from the front don't have to climb over each other.

Here's an idea: what if we moved the entrances to 1/4 and 3/4 of the way through the plane, respectively, rather than placing them at the ends. Pre-boarding, passengers would still be sorted into 2 groups based on how close their seats were to the entrances. During boarding, however, they would effectively be split into 4 groups: those who turned left at the 1st entrance, those who turned right at the 1st entrance, those who turned left at the 2nd entrance, and those who turned right at the 2nd entrance.

<img src="{{ url_for('static', filename='img/plane-boarding/plane-design.png') }}"
     alt="diagram demonstrating passenger grouping"
     class="centered">

This would further reduce the interference between passengers, and, in theory, further reduce boarding times. We're going to run an extremely simple simulation in Python to test this theory.

### Simulating it
The simulation, made using the pygame library, runs in a series of "time steps". It takes 1 time step for a passenger to move between rows, and 5 time steps for a passenger to put their luggage into the rack and take their seat. These numbers were chosen arbitrarily.

We first run a simulation with entrances at the ends of the plane. Then we run a version with centered entrances.

<video width="560" height="315" class="centered" controls>
    <source src="{{ url_for('static', filename='video/plane-boarding/simulation.mp4') }}" type="video/mp4">
</video>

In this case, the entrances-at-ends design takes 239 time steps before everyone is seated, while the centered-entrances design takes 209 time steps, which is 12% less.

### Further testing and results
Just to be scientific about it, we can run both versions of the simulation 10,000 times and see what the average number of time steps is for each. Here are the results:

<img src="{{ url_for('static', filename='img/plane-boarding/plane-simulation-results-graph.png') }}"
     alt="results graph"
     class="centered">

The centered-entrances design is, on average, ~15% faster in this run of the simulation.

Of course, there may be engineering or safety issues that prevent plane manufacturers from playing around with their plane designs like this. Perhaps placing the entrances at the ends gives space for more seats. Perhaps the wings and emergency exits get in the way of centered entrances. Etcetera.

Whatever the case, it's a nice distraction to think about as the trenchcoat stink cloud wafts its way towards you and the baby in your vicinity makes threatening heaving motions.

### Discussion
* [reddit](https://www.reddit.com/r/programming/comments/d1wljy/simulating_plane_designs_for_faster_boarding/)

### Appendix A: follow-up reading
Further thoughts on the efficiency of plane boarding can be enjoyed in this [fun video by CPG Gray](https://www.youtube.com/watch?v=oAHbLRjF0vo), which doesn't mention plane design but discusses a number of algorithms for faster boarding.

### Appendix B: the code
For the curious, [here's the script](https://github.com/Kevinpgalligan/KevingalWebsite/blob/master/experiments/plane-boarding/plane-boarding.py) used to run the simulation and animate it.
