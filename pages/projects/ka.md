name: Ka
link: https://github.com/Kevinpgalligan/ka
type: desktop
date: 2022-03-28
description: Calculator tool with a CLI and GUI.

Ka is a small calculator language for quick, day-to-day calculations. It aims to be convenient: you can start the GUI, do your sums, and close the GUI with Ctrl-W -- no keyboard needed! Or if you're pottering about in the terminal, you can do a quick one-off calculation with `ka '1+1'`.

Featuring...

* A **GUI** and **CLI**.
* **Fractions**: `(5/3) * 3` gives `5`.
* **Units** and unit conversion: `5 ft to m`.
* **Probability** distributions and sampling: `X = Bernoulli(0.3); P(X=1)`.
* **Arrays** with math-like syntax: `{3*x : x in [1,3]}` gives `{3,6,9}`.
* **Lazy combinatorics**: `10000000!/9999999!` gives `10000000` rather than hanging.
* Other boring stuff: Variable assignment. Common math functions and constants.

More examples.

	>>> 2 * (1/2)
	1
	>>> 1 metre + 1 foot to feet
	4.28084
	>>> p = 0.7; C(10,3) * p^3 * (1-p)^7
	0.00900169
	>>> p=.7; N=10; sum({C(N,k)*p^k*(1-p)^(N-k) : k in [0,4]})
	0.047349
	>>> sin(90 deg)
	1 
	>>> e^pi
	23.1407
	>>> X = Binomial(10, 0.3); P(3 <= X < 7)
	0.6066


Installation:

	pip3 install ka-cli
