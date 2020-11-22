title: Recreating grep in Python
date: 2020-10-31
description: A short guide to making Python CLI tools.
draft: yes
requires: code

Let's make a Python version of grep, codenamed dumbgrep. Python's `argparse` package makes it easy to handle the parsing side of things. And using the Python Package Index (PyPI), it's easy to deliver a command line tool into the ravenous hands of the general public.

    $ grep existence </tmp/war-and-peace.txt
    in this part of the house and did not even know of the existence of
    even wish to know of his existence but would now have been offended
    [...]

The full program is available [here](https://github.com/Kevinpgalligan/dumbgrep). If you want to follow along, then you'll need Python 3.

### Baby steps 👶
Here's the skeleton of our project.

    dumbgrep/
    ├── scripts
    │   └── dumbgrep
    ├── setup.py
    └── src
        └── dumbgrepcli
                └── __init__.py

* **dumbgrep** is the Python script that the user will call to use our tool.
* **setup.py** contains the information we need to package our project and upload it to PyPI for distribution.
* **\_\_init\_\_.py** contains the actual functionality of dumbgrep. If you didn't know, a folder that contains a file called **\_\_init\_\_.py** is a Python package. That package's code is saved in the **\_\_init\_\_.py** file. We can access the **\_\_init\_\_.py** code in Python by calling `import dumbgrepcli`.

Why not dump all of our Python code into the **dumbgrep** file? The structure above allows us to split our code into multiple files and even multiple subpackages, which might be useful if it ever grows too big. It's also easier to add tests.

First, let's write the **dumbgrep** script. All it does is call the `main()` function of our `dumbgrepcli` package, which we'll write later.

    :::python
    #!/usr/bin/env python3
    import dumbgrepcli
    dumbgrepcli.main()

The only unusual thing is the so-called [shebang](https://en.wikipedia.org/wiki/Shebang_(Unix)) line at the start, which basically informs Unix-like systems that the script should be run using Python 3.

Next, here's what we might write in **setup.py**. This determines how to build the package and how to upload it to the PyPI.

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

Most of the fields are self-explanatory. "name" is the package's name on PyPI, which must be unique. The file in "scripts" will be installed in a place where the user can call it from the command line.

As an aside: if you add a markdown-formatted README to your project, then a useful trick is to reuse it as the long description of your package on PyPI.

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

That's the boring stuff out of the way! Now we can move on to our fabulous implementation of grep.

### G(lobally search for a) R(egular) E(xpression and) P(rint matching lines)
Here's how we start our implementation of grep in **\_\_init\_\_.py**. We import Python's `argparse` module, which we'll use for argument-parsing. We define the long-awaited `main()` function (and there's boilerplate code at the bottom to call `main()` when we execute this file directly, just so we can test it). Within `main()`, we create an `ArgumentParser`, add a string argument called "pattern" to it, parse the command line arguments, and finally, print out the value of the "pattern" argument.

    :::python
	import argparse
	
	def main():
		parser = argparse.ArgumentParser(description="A replacement for grep.")
		parser.add_argument("pattern", type=str, help="the pattern to search for")
		args = parser.parse_args()
		print(args.pattern)

	if __name__ == "__main__":
		main()

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

All that remains is to code up the logic of grep, which is rather easy in Python, since it has a built-in regex package.

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

We create a `Pattern` object based on the pattern provided by the user. All lines of input that match this pattern are printed to standard output.

And that's it! We've recreated grep. Let's set up a virtual environment where we can install this bad boy and test it out. A virtual environment is a self-contained Python installation that you can experiment on without mucking up your main Python installation.

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

In the next section we'll show off `argparse` a bit more by adding some bells and whistles to dumbgrep.

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

Hurray!

	$ python3 src/dumbgrepcli/__init__.py -v existence </tmp/war-and-peace.txt
	The Project Gutenberg EBook of War and Peace, by Leo Tolstoy
	This eBook is for the use of anyone anywhere at no cost and with almost
	[...]

How about the "--max-count" parameter, which limits the number of lines that grep prints out? This requires an integer parameter, and we count the number of matched lines so that we can exit early once the limit has been reached.

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

Okay, okay. Enough of that. There's one last trick I'd like to share before we move on, however: colour highlighting in the terminal. If we want to highlight the matching part of a line in red, then we can use escape codes to modify font colour in the terminal. First we store the `Match` object returned by `regex.search(...)` in its own variable, since we'll need it later to isolate the part of the line that matches the pattern. We call a new function, `highlight()`, to format the output.

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

### Distribute to the clammering masses
Since we're benevolent and charitable by nature, we obviously want to upload dumbgrep to PyPI. After all, why would anyone want to use the original grep when they could use our version?

    $ time python3 src/dumbgrepcli/__init__.py existence </tmp/war-and-peace.txt >/dev/null
    user    0m0.086s
    $ time grep existence </tmp/war-and-peace.txt >/dev/null
    user    0m0.000s

Oh, right...

Anyway, once our tool is on PyPI, someone can download it by running `pip3 install dumbgrep-cli`. Here's an excellent guide for the whole process: <https://packaging.python.org/tutorials/packaging-projects/>. I won't even bother to list the commands here, since the guide is thorough and it's a simple process.

### Closing thoughts

* Provide template repo.
* End w/ ideas for CLI tools: calculator, show weather, show /download articles from some website
