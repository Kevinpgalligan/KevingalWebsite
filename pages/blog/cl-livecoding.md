title: Graphics livecoding in Common Lisp
date: 2025-01-06
description: Developing a Boids program from scratch without restarting it.
requires: code
tags: lisp programming artsy

Some Lisps, like Common Lisp, have a powerful feature that tends to go underappreciated amidst all the talk about macros: the ability to recompile your program while it's running, without restarting it. For the purposes of this post, and because it sounds cool, let's call this ability *livecoding*[^livecod].

Entering this strange land where the programs never stop, we'll first take a brief tour of Common Lisp and one of its graphics frameworks, Sketch, before ultimately walking through a livecoded implementation of the [Boids algorithm](https://en.wikipedia.org/wiki/Boids) for simulating animal flocking behaviour.

<figure>
<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='img/cl-livecoding/boids-sample.mp4') }}" type="video/mp4">
</video> 
<figcaption>Boids!</figcaption>
</figure>

### "Wait, what exactly is this livecoding thing?"
Consider the typical workflow needed to modify a long-running application like a videogame.

1. Stop the application.
2. Change the code.
3. (If a compiled language) Wait N time units for a full recompilation.
4. Start the application again.
5. Fiddle with the application to get it back to its previous state.
6. Carry on.

In a livecoding environment, the application is never stopped, which immediately eliminates steps 1, 4 and 5. Instead, small code changes are immediately reflected in the running program. Step 3 is often instantaneous because only the changed parts of the program must be recompiled. In theory, then, you can develop an entire application while it continuously runs in the background, without ever waiting for code to recompile. This makes the development process more fluid and interactive, with minimal downtime.

In Common Lisp, the workflow would look something like this:

1. Make a small change to a single function.
2. Recompile the function (instantaneous).
3. Carry on.

For an example of this workflow in action, check out Common Lisp and Emacs being used as an environment for [live musical performance](https://www.youtube.com/watch?v=EkYUU0UoB_0). You can hear about a Lisp program being debugged remotely while running in [deep space](https://corecursive.com/lisp-in-space-with-ron-garret/). Livecoding (or hot reloading, or whatever you like to call it) is also available in other languages, like Smalltalk and Erlang.

### A rough sketch of Sketch
Before jumping into Boids, let's take a brief look at [Sketch](https://github.com/vydd/sketch), our Common Lisp graphics framework of choice. If you've ever used Processing or p5js, then the code should look familiar, but today we're more concerned with the big ideas than with understanding the minutiae of the code.

The main interface to Sketch is the `defsketch` macro. The code below defines a "sketch" called `my-sketch`.

	:::lisp
	(defsketch my-sketch
        ((width 200)
         (height 200)
         (n 5))
      ;; ...drawing code here...
	  )

After the name of the sketch comes a list of bindings that define its state and configuration. Here, the configuration parameters `width` and `height`, which determine the dimensions of the window, are set to `200`, while `n` is an attribute we've added for our own use.

Then comes the drawing code. This gets run in a loop while the sketch is running, once per frame. The following snippet draws 5 red circles on a black background, each of radius 10 and in random positions.

	:::lisp
    (background +black+)
    (loop repeat n
          do (with-pen (make-pen :fill +red+)
               (circle (random width) (random height) 10)))

After painting the background black, the all-powerful `loop` macro is used to draw `n` circles. The `with-pen` macro (defined by Sketch) configures drawing properties like fill colour, stroke width and stroke colour. It takes a "pen" object as an argument.

Here's all the code together:

	:::lisp
	(defsketch my-sketch
        ((width 200)
         (height 200)
         (n 5))
      (background +black+)
      (loop repeat n
	        do (with-pen (make-pen :fill +red+)
	             (circle (random width) (random height) 10))))

Finally, to run the sketch, we compile our code and execute `(run-sketch 'my-sketch)` from the REPL, resulting in...

<figure>
<img src="{{ url_for('static', filename='img/cl-livecoding/sketch-sample.gif') }}"
     alt="The result: red circles flashing on a black background."
     class="centered">
<figcaption>...art.</figcaption>
</figure>

That's all we need to know about Sketch for now!

### Livecoding Boids
[Boids](https://en.wikipedia.org/wiki/Boids) is an algorithm from 1986 for simulating flocks of birds. In its essence, it consists of applying 3 forces to the simulated birds. Quoting Wikipedia, these forces are:

> * separation: steer to avoid crowding local flockmates
> * cohesion: steer to move towards the average position (center of mass) of local flockmates
> * alignment: steer towards the average heading of local flockmates

How can we implement this ourselves? First, we need a canvas to draw on!

    :::lisp
	(defsketch boids
        ((width 400)
         (height 400)
         (restart-on-change nil))
	  (background (gray-255 230))

The only mysterious thing in this code is the `restart-on-change` parameter, which is available in my [fork](https://github.com/Kevinpgalligan/sketch) of Sketch. When its value is `nil` (false), the sketch's state - like the boid positions - won't be reset when we recompile our code.

Compiling the defsketch form in Emacs (with the Ctrl-C Ctrl-C shortcut) and executing `(run-sketch 'boids)` at the REPL gives us... 🥁... a gray background. Wonderful.

<figure>
<img src="{{ url_for('static', filename='img/cl-livecoding/boids-1-canvas.png') }}"
     alt="A light-gray background."
     class="centered">
</figure>

(Note: all going well, this modest window will run continuously throughout the entire development lifecycle).

Now let's create some boids to populate our world. We add a `boid` class to store their position and velocity, as well as a convenience function `make-boid` to create a boid from x & y co-ordinates. These rely on a hopefully self-explanatory implementation of 2d vectors, which are created using the `vec2` function.

	:::lisp
	(defclass boid ()
	  ((pos :initarg :pos :accessor pos)
	   (velocity :initarg :velocity
				 :initform (vec2 0 0)
				 :accessor velocity)))
	
	(defun make-boid (x y)
	  (make-instance 'boid :pos (vec2 x y)))

To the sketch itself, we add 20 boids in random positions, and pass them to the `draw-boids` function in the drawing loop.

	:::lisp hl_lines="5 6 8"
	(defsketch boids
		((width 400)
		 (height 400)
		 (restart-on-change nil)
		 (boids (loop repeat 20
					  collect (make-boid (random width) (random height)))))
	  (background (gray-255 230))
	  (draw-boids boids))

If we then recompile defsketch (with Ctrl-C Ctrl-C)... 

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-2-missing-draw-compile.mp4') }}" type="video/mp4">
</video> 

...we get an error! Woops. 

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-2-missing-draw.mp4') }}" type="video/mp4">
</video> 

But of course! We forgot to define `draw-boids`. The program doesn't crash, however, and we'll soon be able to recover from this setback.

Here's an implementation of `draw-boids`. We don't need to get into the weeds of how it works. For each boid, it does some unwieldy vector math to figure out which direction the boid is facing and draws a triangle pointing in that direction.

	:::lisp
	(defun draw-boids (boids)
	  (let ((boid-width 10)
			(boid-length 20))
		(loop for boid in boids
			  do (with-slots (pos velocity) boid
				   (with-pen (:fill +black+)
					 (let* ((dir (if (zerop (v-length velocity))
									 (vec2 0 -1)
									 (v-normalise velocity)))
							(p1 (v+ pos (v-rescale (/ boid-length 2) dir)))
							(p2 (v+ pos
									(v-rescale (- (/ boid-length 2)) dir)
									(v-rescale (/ boid-width 2)
											   (perpendicular-anticlockwise dir))))
							(p3 (v+ pos
									(v-rescale (- (/ boid-length 2)) dir)
									(v-rescale (/ boid-width 2)
											   (perpendicular-clockwise dir)))))
					   (polygon (vx p1) (vy p1)
								(vx p2) (vy p2)
								(vx p3) (vy p3))))))))

As soon as we compile `draw-boids`, the error screen disappears and our lovely boids are drawn into place. And we didn't have to restart the program to fix it!

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-3-define-draw.mp4') }}" type="video/mp4">
</video> 

There are two Common Lisp features that enable us to fix errors on-the-fly like we've done here:

1. Newly compiled code, and recompiled code, is immediately loaded into the running program (sometimes called "hot reloading"). This opens up possibilities such as optimising your running program, tweaking parameters like gravitational force and background colour, and iteratively developing a GUI.
2. The condition system! This is somewhat like exception handling in other languages, but more powerful. Not only can we signal exceptional situations ("conditions"), but we can also define "restarts" for recovering from those situations. When a running Common Lisp program encounters an unhandled condition, control passes to the debugger, and the user is presented with a selection of restarts. Perhaps they want to recompile the offending function and continue execution from the previous stack frame. Or perhaps the error was a division by zero, and the offending function provides a restart that swaps in a value of 1 for the divisor. Suddenly, there are a lot more possibilities than just crashing the program.

Anyway, a worthy discussion of the condition system would take up a full blog post of its own. Back to Boids!

Now that our boids are drawn correctly, we want them to move around and do boid things. First, we implement an `update-state` function, which basically adds the velocity of each boid to its position (so that the boid moves), and applies the 3 Boidian forces to update the velocity. For now, the functions implementing the forces are stubbed out -- they return all-zero vectors so that they have no effect on the velocity.

    :::lisp
    (defun update-positions (boids)
      (let ((max-velocity 10))
        ;; Update boid positions.
        (map nil
             (lambda (boid)
               (setf (pos boid) (v+ (pos boid) (velocity boid))))
             boids)

        ;; Update boid velocities.
        (loop for boid in boids
              do (setf (velocity boid)
                       (v-clamp max-velocity
                                (v+ (velocity boid)
                                    (rule1 boid boids)
                                    (rule2 boid boids)
                                    (rule3 boid boids)))))))

    ;; Stubs! (For now).
    (defun rule1 (boid boids)
      (vec2 0 0))

    (defun rule2 (boid boids)
      (vec2 0 0))

    (defun rule3 (boid boids)
      (vec2 0 0))

We then have to modify the drawing loop to call `update-state`.

	:::lisp hl_lines="10"
	(defsketch boids
		((width 400)
		 (height 400)
		 (restart-on-change nil)
		 (boids (loop repeat 20
					  collect (make-boid (random width)
										 (random height)))))
	  (background (gray-255 230))
	  (draw-boids boids)
	  (update-positions boids))

So far, these changes won't affect the boid behaviour, because the force implementations are stubbed. Let's circle back and implement `rule-1`, which can be summarised as "stay away from other boids". When a boid is less than 10 pixels from another boid, we push them away from each other to avoid crowding.

    :::lisp hl_lines="2 3 4 5 6 7 8 9 10"
    (defun rule1 (boid boids)
      (let ((v-sum (vec2 0 0)))
       (loop for boid2 in boids
             for offset = (v- (pos boid) (pos boid2))
             for dist = (v-length offset)
             when (and (not (eq boid boid2)) (< dist 10))
               do (v+! v-sum offset))
       v-sum))

(Note: the vector functions ending in `!`, like `v+!`, follow the convention of overwriting the first argument with the result).

When we recompile this function...

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-4-first-rule.mp4') }}" type="video/mp4">
</video> 

...two boids that happen to be too close are sent flying off into the void. There's no counterforce to bring them back, just yet.

Next, we implement `rule-2`: boids should fly towards the average position of other boids. Our implementation could be more efficient by summing the boid positions just once, rather than doing it for every single boid, but no matter.

    :::lisp hl_lines="2 3 4 5 6 7 8 9 10 11"
    (defun rule2 (boid boids)
      (let ((center (vec2 0 0)))
        (map nil
             (lambda (boid2)
               (when (not (eq boid boid2))
                 (v+! center (pos boid2))))
             boids)
        (v-scale! (/ (1- (length boids))) center)
        (v-! center (pos boid))
        (v-scale! (/ 200) center)
        center))

Recompiling `rule-2`, we immediately see...

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-5-second-rule.mp4') }}" type="video/mp4">
</video> 

Yes! This is starting to look vaguely Boids-like. Let's add the final rule, `rule-3`: match velocity with other boids. (Implementation note: we probably shouldn't update the velocities until all the new velocities have been calculated, but oh well).

    :::lisp hl_lines="2 3 4 5 6 7 8 9 10 11"
    (defun rule3 (boid boids)
      (let ((result (vec2 0 0)))
        (map nil
             (lambda (boid2)
               (when (not (eq boid boid2))
                 (v+! result (velocity boid2))))
             boids)
        (v-scale! (/ (1- (length boids))) result)
        (v-! result (velocity boid))
        (v-scale! (/ 8) result)
        result))

Recompiling, we see the Boids calm down a little bit.

<video loop autoplay muted class="centered">
  <source src="{{ url_for('static', filename='video/cl-livecoding/boids-6-third-rule.mp4') }}" type="video/mp4">
</video> 

Since it's not very bird-like to fly around in a vortex of death, we could also give the boids a purpose by making them follow the mouse position, with the following changes.

    :::lisp hl_lines="8 11 13 14 29"
    (defsketch boids
        ((width 400)
         (height 400)
         (restart-on-change nil)
         (boids (loop repeat 20
                      collect (make-boid (random width)
                                         (random height))))
         (mouse-pos (vec2 200 200)))
      (background (gray-255 230))
      (draw-boids boids)
      (update-positions boids mouse-pos))

    (defmethod on-hover ((instance boids) x y)
      (setf (boids-mouse-pos instance) (vec2 x y)))

    (defun update-positions (boids mouse-pos)
      (let ((max-velocity 10))
        (map nil
             (lambda (boid)
               (setf (pos boid) (v+ (pos boid) (velocity boid))))
             boids)
        (loop for boid in boids
              do (setf (velocity boid)
                       (v-clamp max-velocity
                                (v+ (velocity boid)
                                    (rule1 boid boids)
                                    (rule2 boid boids)
                                    (rule3 boid boids)
                                    (v-rescale 0.1 (v- mouse-pos (pos boid)))))))))




And with that, we have a complete implementation of Boids! At the risk of beating a dead horse, I'll re-emphasise that we did the whole thing without once restarting our program or waiting for code to compile.

### Closing thoughts
I hope, in this brief demonstration of livecoding, I've given you a taste of how useful and fun this feature can be. Like I said, it's not unique to Common Lisp, as at least Smalltalk and Erlang have similar capabilities. It's also possible to bridge the gap in less interactive languages by making applications automatically restart themselves when a code change is detected, or by bolting on a scripting language. Just do me a favour and ask yourself, the next time you're waiting the requisite time units for your code to recompile: *How can I make this workflow more interactive? How can I make it more... like Common Lisp?*

TODO: 1. add link to Learn X in Y Mins, 2. tweak diff colour.

<!-- Just in case, here's the command I used to cut the screen recordings:
ffmpeg -i input.mp4 -ss 2 -to 4 -async 1 cut.mp4 -->


[^livecod]: See the [Wiki page](https://en.wikipedia.org/wiki/Live_coding), and also [interactive programming](https://en.wikipedia.org/wiki/Interactive_programming).
