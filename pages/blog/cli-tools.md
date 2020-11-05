title: Making CLI tools in Python, from argparsing to packaging
date: 2020-10-31
description: How to 
imgthumbnail: img/missing.jpg
draft: yes
requires: code

Something something. Everyone loves grep. Everyone loves cowsay. How easy is it to make your own CLI application and get it into people's hands? Well, if you use Python, it's very easy.

The nice thing about using Python for a CLI application is that it has built-in argparsing, and also a distribution network (PyPI). It's really easy to publish your CLI application to PyPI and then it's available to anyone with an installation of pip.

### Parsing program args
Python comes with a built-in argument-parsing module, argparse. It's rather easy to use.

    :::python
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--blah")

### Getting it out there broh
Useful link: https://packaging.python.org/tutorials/packaging-projects/

Then go through packaging & publishing to PyPI.

### Fuck
* Provide template repo at the end.
* End w/ ideas for CLI tools: calculator, show weather, show /download articles from some website
