title: Why doesn't the Moon crash into Earth?
date: 2021-07-04
description: Because it keeps missing.
imgthumbnail: img/mooncrash/thumbnail.jpg
requires: math
publish: y
tags: simulation

This is one of the questions explored by Bill Bryson in *[A Short History of Nearly Everything](/blog/shorthistory.html)*. It's a good question. Isn't there a force called gravity that keeps everything anchored to Earth? Yes, there is. And shouldn't that same force bring the Moon on a collision course directly towards us? Well, no. As we can all attest to, rather than crashing into Earth and sending us all to oblivion, the Moon continues to orbit us as it has done for the past billion-odd years.

To explain this strange state of affairs, Bryson uses an analogy that I really like. Imagine a cannon on top of a mountain. If you fire the cannon with minimum power, the cannonball will quickly fall to the ground under the pull of gravity.

<img src="{{ url_for('static', filename='img/mooncrash/minimum-power.png') }}"
     alt="A cannon is fired from a mountain with a minimum amount of power and falls straight to the ground."
     class="centered">

If you give it more juice, the cannonball will travel further before hitting the ground. In fact, it might even travel *around* the earth somewhat, since, to the best of my knowledge, the earth isn't flat.

<img src="{{ url_for('static', filename='img/mooncrash/more-power.png') }}"
     alt="Cannon is fired with slightly more power, cannonball travels some of the way around Earth as it falls to the ground."
     class="centered">

If you fire the cannon with **rocket launch** levels of power, then the cannonball will escape Earth's gravitational pull and fly off into space.

<img src="{{ url_for('static', filename='img/mooncrash/max-power.png') }}"
     alt="Cannon is fired with soooo much power that it escapes Earth's gravitational pull."
     class="centered">

Finally, what will happen if you fire the cannon with enough power that the cannonball won't fall to Earth, but not enough power that it can escape the pull of gravity? What will happen is that it will fall towards Earth, and *miss*. It will begin to orbit.

<img src="{{ url_for('static', filename='img/mooncrash/orbit-power.png') }}"
     alt="Cannon is fired with just enough power so that the cannonball starts to orbit Earth."
     class="centered">

It's kind of amazing. The Moon is constantly falling towards Earth, and missing. This dance will continue [forever](https://www.spaceanswers.com/solar-system/will-the-moon-ever-leave-earths-orbit/), apparently, or at least until the Sun explodes and destroys everything.

I'm writing about this today because I wanted to try the experiment for myself. Since there aren't any mountains nearby, and I don't have access to a sufficiently powerful cannon, I resorted to simulating it instead. Here's a demo.

<video width="560" height="315" class="centered" controls>
    <source src="{{ url_for('static', filename='video/mooncrash/sample.mp4') }}" type="video/mp4">
</video>

There aren't many details to explain. The Moon is fired as if from a cannon. Besides the initial velocity, its trajectory is determined by 3 physical phenomena:

(1) Gravitational acceleration, given by the formula

```math
a = \frac{Gm_1m_2}{r^2}.
```

$`G`$ is the [gravitational constant](https://en.wikipedia.org/wiki/Gravitational_constant). $`m_1`$ and $`m_2`$ are the masses of Earth and the Moon. And $`r`$ is the distance between them.

(2) Normal force. Like our ol' buddy Isaac Newton said, the collision of two objects due to some force causes an equal and opposite force, which stops the objects from passing through each other. To calculate this, you take the component of the Moon's velocity that points directly towards Earth and reverse it.

(3) Friction. The Moon has to slow down as it rolls along the Earth, as otherwise there are hilarious consequences. Take the component of the Moon's velocity that is tangential to Earth, reverse it, and scale it by your preferred friction coefficient.

Final thing to note: the Moon and Earth in the simulation are the same size relative to each other as they are in real life, which I find neat to see.

The code is [here](https://github.com/Kevinpgalligan/mooncrash). If you come back later, I might have uploaded the simulation to itch.io, since getting it to run from the source code is a pain if you don't already have a Common Lisp setup.
