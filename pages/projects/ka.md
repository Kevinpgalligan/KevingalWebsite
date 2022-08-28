name: ka
link: https://github.com/Kevinpgalligan/ka
type: desktop
date: 2022-03-28
description: A calculator tool with a CLI and GUI.

ka is a small calculator language. It supports various useful features for day-to-day calculations, such as:

* Common math functions and constants.
* Fractions.
* Units and unit conversion.
* Variable assignment.

There are 3 ways to interact with it: executing individual expressions through the CLI (`ka '1+1'`), a CLI interpreter (`ka`), and a GUI (`ka --gui`).

    >>> 2 * (1/2)
    1
    >>> 1 metre + 1 foot > feet
    4.2808398950131235
    >>> p = 0.7; C(10,3) * p^3 * (1-p)^7
    0.009001692000000007
    >>> sin(90 deg)
    1 
    >>> e^pi
    23.140692632779263

Installation:

	pip3 install ka-cli
