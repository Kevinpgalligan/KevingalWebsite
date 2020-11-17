title: Making your own CLI tool in Python is friggin easy
date: 2020-10-31
description: A short guide to Python CLI tools.
draft: yes
requires: code

grep is the everyman's software tool, used by programmers everywhere to search text files for lines that match a particular pattern. While it works perfectly fine, how would you go about making your own version of grep? Or, for that matter, how would you go about creating a different tool, like something to print a summary of your local weather website, or to download frontpage articles from a news website?

In Python, it's incredibly easy. Python comes with a package, `argparse`, for parsing CLI (Command Line Interface) arguments. The Python community also has a distribution network, PyPI (Python Package Interface), that allows you to get your CLI tool into the hands of users of any operating system you can imagine, as long as they have Python installed.

This article will show you how to develop your own version of grep - dumbgrep - from scratch. From argument-parsing to distribution.

### Baby steps 👶
First, I'll assume that you've installed Python 3 on your system.

Second, here's the skeleton of our project. 

    dumbgrep/
    ├── scripts
    │   └── dumbgrep
    ├── setup.py
    └── src
        └── dumbgrepcli
                └── __init__.py

And here's the purpose of all these files:

* **dumbgrep** is the Python script that the user will call to use our tool.
* **setup.py** contains all of the information we need to package our project and upload it to PyPI for distribution.
* **\_\_init\_\_.py** contains the actual functionality of dumbgrep. If you didn't know, a folder that contains a file called **\_\_init\_\_.py** is a Python package. That package's code is saved in the **\_\_init\_\_.py** file. So we can access all of the dumbgrep code by calling `import dumbgrepcli` in Python.

Why not dump all of our Python code into the **dumbgrep** file? The structure I've given above allows us to split our code into multiple files and even multiple subpackages, which might be useful if the code ever grows past ~100 lines. It's also easier to add tests with this structure.

Straight away, let's write the **dumbgrep** script. All it does is call the `main()` function of our `dumbgrepcli` package, which we'll write later.

    :::python
    #!/usr/bin/env python3
    import dumbgrepcli
    dumbgrepcli.main()

The only unusual thing is the so-called "shebang" line at the start, which basically informs Unix-like systems that the script should be run using Python 3.

Next, here's what **setup.py** might look like.

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

The key fields here are:

* "name", the name of your package on PyPI.
* "description", a 1-line description of your package.
* "version"?
* "install_requires", a list of dependencies (we don't have any).

As an aside, if you want to add a markdown-formatted README to your project, then a useful trick is to reuse it as the long description of your package on PyPI.

	:::python hl_lines="2 3 7 8"
	...
	with open("README.md", "r") as f:
		long_description = f.read()

	setup(
		...
		long_description=long_description,
		long_description_content_type="text/markdown",
		...)

That's the boring stuff out of the way! Now we can move on to our fabulous implementation of grep.

### G(lobally search for a) R(egular) E(xpression and) P(rint matching lines)
Here's how we start our implementation of grep in **\_\_init\_\_.py**. We import Python's `argparse` module, which we'll use for argument parsing. We define the long-awaited `main()` function (and there's boilerplate code at the bottom to call `main()` when we execute this file). Within `main()`, we create an `ArgumentParser`, add a string argument called "pattern" to it, parse the command-line arguments, and finally, print out the value of the "pattern" argument.

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

All that remains is to code up the logic of grep, which is rather easy in Python, since it comes with a regex implementation.

### Add milk and sugar
Highlight match. Inverse match.

### Distribute to the clammering masses
Useful link: https://packaging.python.org/tutorials/packaging-projects/

### Closing thoughts
* Provide template repo.
* End w/ ideas for CLI tools: calculator, show weather, show /download articles from some website
