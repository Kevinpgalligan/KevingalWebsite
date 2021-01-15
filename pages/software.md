title: kevingal/software

## Software
Below is a selection of free software I've developed, typically because it's useful or amusing to me.

Key:

* 🌎 = web app.
* 📥 = desktop app.

Navigation list:

[TOC]

<hr>

### [Emojipasta Generator](/apps/emojipasta.html) 🌎
Add word-relevant 🤖 emojis ✨ to text with sometimes hilarious results. Based on [AnEmojipastaBot](https://github.com/Kevinpgalligan/EmojipastaBot), which I unleashed on Reddit a few years ago.

### [Pixelate](/apps/pixelate.html) 🌎
Make images more pixely, make them black & white, or resize them. I used it for [this](/blog/mona-lisa-gol.html) blog post.

![dog from CIFAR-10, I think](/static/img/pixelate/dog-before.png)![pixelated version](/static/img/pixelate/dog-after.png)![black & white version](/static/img/pixelate/dog-after-bw.png)

### [Tiny Metronome](/apps/metronome.html) 🌎
A mobile-friendly, 981-byte metronome. I made it because most of the metronome web apps I found on the internet were bloated, or clunky on mobile. I also wrote about this app in a [blog post](/blog/metronome.html).

### [bs](https://github.com/Kevinpgalligan/bs) 📥
A Python CLI tool that converts numbers between bases with as little typing as possible. If you don't specify the base, it does all valid conversions between common ones.

Installation:

    pip3 install base-convert-cli

Examples (more in the README):

	$ bs FFFE
	[from hexadecimal]
	  decimal     65534
	  binary      1111111111111110
	  octal       177776
	$ bs -t d F
	15

### [pseu](https://github.com/Kevinpgalligan/pseu) 📥
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
