## KevingalWebsite
This package is used to generate the static files for kevingal.com, which are then served by GitHub Pages from the GitHub repository [kevinpgalligan.github.io](https://github.com/Kevinpgalligan/kevinpgalligan.github.io).

It comes with a number of scripts to automate the whole process:

## Setup
* Create virtual environment: `python3 -m venv venv`.
* Activate it: `source venv/bin/activate`.
* Install dependencies: `pip3 install -r requirements.txt`.

## Usage
First, make sure the virtual environment is activated: `source venv/bin/activate`.

Run debug server with: `python3 -m app`.

Various options to generate/dump a static copy of the website:

* All website files... `python3 app.py --all`.
* Just one file, at the given path... `python3 app.py /file.html`.
* Files that have changed since the last build... `python3 app.py --selective`.

Finally! Copy files to kevinpgalligan.github.io repository: `cp -r build/* /path/to/kevinpgalligan.github.io/`.

## Requirements
python3, pip3, setuptools (install through pip3). Maybe some other stuff, the setup script isn't as bulletproof as I would like.
