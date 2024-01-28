name: pseu
link: https://github.com/Kevinpgalligan/pseu
type: desktop
date: 2020-10-31
description: A CLI tool with random number utilities.

A Python CLI tool that randomly generates numbers, shuffles, picks, and rolls dice. Possible uses: picking someone to do a shitty job; relieving yourself of the agony of deciding which movie to watch; making life choices.

Installation:

	pip3 install pseu-cli

Examples (more in the README):

	$ pseu pick "good life choice" "bad life choice"
	bad life choice
	$ pseu pick --n 2 </tmp/movies.txt
	Boogie Nights
	The Hunt for the Wilderpeople
	$ pseu roll 1d6
	3
	$ pseu rand 100
	42
	$ pseu shuffle alice sue bob
	bob
	sue
	alice
