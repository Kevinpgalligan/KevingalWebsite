title: Recreating grep in Python
date: 2020-11-27
description: A guide to making Python CLI tools.
requires: code
imgthumbnail: img/cli-tools/thumbnail.jpg

Let's make our own version of grep, nicknamed dumbgrep. Along the way, we'll learn about 19th-century Russian literature and how to make command line interface (CLI) tools in Python.

    $ grep existence </tmp/war-and-peace.txt
    in this part of the house and did not even know of the existence of
    even wish to know of his existence but would now have been offended
    [...]

Why Python? Python's `argparse` package makes it easy to handle the parsing side of things. And using the Python Package Index (PyPI), you can easily deliver a CLI tool to the writhing masses of humanity.

### Baby steps 👶
You'll need Python 3 if you want to follow along. For reference, the full program is available [here](https://github.com/Kevinpgalligan/dumbgrep). Shamefully, I've only tested it on Linux, so there might be extra hoop-jumping required to set it up on Windows.

Here's the skeleton of our project.

    dumbgrep/
    ├── scripts
    │   └── dumbgrep
    ├── setup.py
    └── src
        └── dumbgrepcli
                └── __init__.py

* **dumbgrep** is the Python script that a user can call from the CLI.
* **setup.py** contains the information we need to package our project and upload it to PyPI for distribution.
* **\_\_init\_\_.py** contains the actual functionality of dumbgrep. If you didn't know, a folder that contains a file called **\_\_init\_\_.py** is a Python package. That package's code is saved in the **\_\_init\_\_.py** file.

Why not dump all of our Python code into the **dumbgrep** file? This more complicated structure allows us to split the code into multiple files and even multiple subpackages, which will be useful if the codebase grows too big. It's also easier to add tests this way, for boring people who write tests.

Let's write the **dumbgrep** script. All it does is call the `main()` function of the `dumbgrepcli` package, which we'll write later.

    :::python
    #!/usr/bin/env python3
    import dumbgrepcli
    dumbgrepcli.main()

The only thing about this that might possibly be unusual to a Python afficionado is the so-called [shebang](https://en.wikipedia.org/wiki/Shebang_(Unix)) line at the start, which basically informs Unix-like systems that the script should be run using Python 3.

Next, here's what we might write in **setup.py**. This determines how to build the package and how to upload it to PyPI.

    :::python
	from setuptools import setup
	from setuptools import find_packages

	setup(
		name="dumbgrep-cli",
		description="The best grep I ever did see.",
		long_description="The best grep I ever did see.",
		version="1.0.0",
		url="https://github.com/Kevinpgalligan/dumbgrep",
		author="Kevin Galligan",
		author_email="galligankevinp@gmail.com",
		scripts=["scripts/dumbgrep"],
		packages=find_packages("src"),
		package_dir={'': 'src'},
		classifiers=[
			"Programming Language :: Python :: 3",
			"License :: OSI Approved :: MIT License"
		],
		install_requires=[]
	)

Most of the fields are self-explanatory. "name" is the package's name on PyPI, which must be unique. The files under the "scripts" field will be installed to a place where the user can call them from the command line.

As an aside: if you add a Markdown-formatted README to your project, then a useful trick is to reuse it as the long description of your package on PyPI.

	:::python hl_lines="2 3 7 8"
	...
	with open("README.md", "r") as f:
		long_description = f.read()

	setup(
		...
		long_description=long_description,
		long_description_content_type="text/markdown",
		...
    )

That's the boring stuff out of the way! Now we can move on to plagiarising grep.

### G(lobally search for a) R(egular) E(xpression and) P(rint matching lines)
Here's how we start our implementation of grep in **\_\_init\_\_.py**.

    :::python
	import argparse
	
	def main():
		parser = argparse.ArgumentParser(description="A replacement for grep.")
		parser.add_argument("pattern", type=str, help="the pattern to search for")
		args = parser.parse_args()
		print(args.pattern)

	if __name__ == "__main__":
		main()

We import Python's `argparse` module, which we'll use for argument-parsing. We define the long-awaited `main()` function. There's boilerplate code at the bottom that calls `main()` when we execute the file directly, just so we can test it. Within `main()`, we create an `ArgumentParser`, add a string argument called "pattern" to it, parse the command line arguments, and finally, print out the value of the "pattern" argument.

This already gets us a lot of stuff. We have nicely-formatted help, by default.

	$ python3 src/dumbgrepcli/__init__.py -h
	usage: __init__.py [-h] pattern

	A replacement for grep.

	positional arguments:
	  pattern     the pattern to search for

	optional arguments:
	  -h, --help  show this help message and exit

If a user forgets to provide a pattern, they get a nice error message.

	$ python3 src/dumbgrepcli/__init__.py
	usage: __init__.py [-h] pattern
	__init__.py: error: the following arguments are required: pattern

And we can access the value of the "pattern" argument through `args.pattern`.

	$ python3 src/dumbgrepcli/__init__.py hello
	hello

All that remains is to code up the logic of grep. This is rather easy in Python, since it has a built-in regex package.

	:::python hl_lines="2 3 9 10 11 12"
	import argparse
	import sys
	import re

	def main():
		parser = argparse.ArgumentParser(description="A replacement for grep.")
		parser.add_argument("pattern", type=str, help="the pattern to search for")
		args = parser.parse_args()
		regex = re.compile(args.pattern)
		for line in sys.stdin:
			if regex.search(line):
                sys.stdout.write(line)

	if __name__ == "__main__":
		main()

We create a `Pattern` object based on the pattern provided by the user, and all lines of input that match this pattern are printed to standard output.

And that's it! We've recreated grep. Let's set up a virtual environment where we can install this bad boy and test it out. (A virtual environment is a self-contained Python installation that you can experiment on without mucking up your main Python installation).

##### Create and activate the environment

	$ pwd
	/home/kevingal/proyectos/dumbgrep
	$ mkdir env
	$ python3 -m venv env/
	$ source env/bin/activate
	(env) $ which python3
	/home/kevingal/proyectos/dumbgrep/env/bin/python3
	
##### Install dumbgrep

	(env) $ python3 setup.py install
	running install
	running bdist_egg
	running egg_info
	[...]
	(env) $ which dumbgrep
	/home/kevingal/proyectos/dumbgrep/env/bin/dumbgrep

##### Test it out, then deactivate the environment

	(env) $ dumbgrep existence </tmp/war-and-peace.txt
	in this part of the house and did not even know of the existence of
	even wish to know of his existence but would now have been offended
	[...]
	(env) $ deactivate
	$

In the next section we'll explore `argparse` a bit more by adding some bells and whistles to dumbgrep.

### Milk and sugar
Let's say we want to recreate grep's "-v" flag, which means that only lines NOT matching the input pattern are printed. All we have to do is add a boolean flag to our argument parser to check whether we should invert the matches. And then tweak the matching logic to use that flag.

    :::python hl_lines="4 5 9"
	...
	def main():
		...
		parser.add_argument("-v", dest="invert", default=False,
			action="store_true", help="invert matches")
		args = parser.parse_args()
		regex = re.compile(args.pattern)
		for line in sys.stdin:
			if args.invert != bool(regex.search(line)):
				sys.stdout.write(line)

Yippee.

	$ python3 src/dumbgrepcli/__init__.py -v existence </tmp/war-and-peace.txt
	The Project Gutenberg EBook of War and Peace, by Leo Tolstoy
	This eBook is for the use of anyone anywhere at no cost and with almost
	[...]

How about the "--max-count" parameter, which limits the number of lines that grep prints out? We accept the limit as an integer argument, and count the number of matched lines so that we can exit early once the limit has been reached.

	:::python hl_lines="2 6 7 9 12 14 15"
	...
	import math

	def main():
		...
	    parser.add_argument("--max-count", "-m", type=int,
			default=math.inf, help="max number of matches to print")
		...
		matches = 0
		for line in sys.stdin:
			if args.invert != bool(regex.search(line)):
				matches += 1
				sys.stdout.write(line)
			if matches >= args.max_count:
				break

It works!

	$ python3 src/dumbgrepcli/__init__.py -m 1 existence </tmp/war-and-peace.txt
	in this part of the house and did not even know of the existence of
	$

Okay, okay. That's enough of that. There's one last trick I'd like to share before we finish, however: colour highlighting in the terminal. If we want to highlight the matching part of a line, then we can use escape codes to modify font colour in the terminal. First we store the `Match` object returned by `regex.search(...)` in its own variable, since we'll need it later to isolate the part of the line that matches the pattern. And we call a new function, `highlight()`, to format the output.

	:::python hl_lines="4 5 7"
	def main():
		...
		for line in sys.stdin:
			match = regex.search(line)
			if args.invert != bool(match):
				...
				sys.stdout.write(highlight(match, line))
			...
 
Here's the `highlight()` function. Main things to note: 1) to avoid having ugly escape codes in our output when we write to a file, we check whether we're writing to a terminal through `sys.stdout.isatty()`; 2) the first escape code we write changes the colour of all following text to red, and it's only after we write the reset escape code that this effect is undone.

	:::python
	def highlight(match, line):
		if not match or not sys.stdout.isatty():
			return line
		return (line[:match.start()]
			+ "\033[31m" # change to red
			+ line[match.start():match.end()]
			+ "\033[0m" # reset
			+ line[match.end():])

And the result:
<figure>

<img src="{{ url_for('static', filename='img/cli-tools/existence.png') }}"
     alt="Highlighted text from War & Peace, output of dumbgrep in the terminal."
     class="centered">

<figcaption>Heavy stuff.</figcaption>
</figure>

### Distribute to the clammering public
If we're feeling particularly benevolent and charitable, then we can upload our nifty tool to PyPI. After all, why would anyone want to use the original grep when they could use our version?

    $ time python3 src/dumbgrepcli/__init__.py existence </tmp/war-and-peace.txt >/dev/null
    user    0m0.086s
    $ time grep existence </tmp/war-and-peace.txt >/dev/null
    user    0m0.000s

Oh, right...

Anyway, here's an excellent guide that describes the whole process: <https://packaging.python.org/tutorials/packaging-projects/>. There's no point in duplicating the instructions here, since the guide is thorough and straightforward. Once dumbgrep is on PyPI, anyone can download it by running `pip3 install dumbgrep-cli`, as per the package name we defined in **setup.py**.

That's it. The full dumbgrep code is available [here](https://github.com/Kevinpgalligan/dumbgrep). You can use it as a template for your own CLI tools. I've also created 2 actually kinda useful CLI tools that you can check out for inspiration: [pseu](https://github.com/Kevinpgalligan/pseu) and [bs](https://github.com/Kevinpgalligan/bs).

    $ pseu pick "good life choice" "bad life choice"
    bad life choice
    $ pseu roll 1d6
    3
    $ bs FFFE
    [from hexadecimal]
      decimal     65534
      binary      1111111111111110
      octal       177776
