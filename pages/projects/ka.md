name: ka
link: https://github.com/Kevinpgalligan/ka
type: desktop
date: 2022-03-28
description: Calculator tool with a CLI and GUI.

ka is a small calculator language. It supports various useful features for day-to-day calculations, such as:

* Common math functions and constants.
* Fractions.
* Units and unit conversion.
* Variable assignment.
* Probability distributions and sampling.

There are 3 ways to interact with it: executing individual expressions through the CLI (`ka '1+1'`), a CLI interpreter (`ka`), and a GUI (`ka --gui`).

	>>> 2 * (1/2)
	1
	>>> 1 metre + 1 foot to feet
	4.28084
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
