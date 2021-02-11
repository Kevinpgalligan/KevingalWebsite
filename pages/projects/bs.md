name: bs
link: https://github.com/Kevinpgalligan/bs
type: desktop
order: 4

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
