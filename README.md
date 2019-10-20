## Description
This package is used to generate the static files for kevingal.com, which are then served by GitHub Pages from the GitHub repository [kevinpgalligan.github.io](https://github.com/Kevinpgalligan/kevinpgalligan.github.io). It comes with a number of scripts to automate this process: generating the static files, moving them to the local copy of the kevinpgalligan.github.io repository, displaying a diff to the user, and finally deploying to GitHub Pages.

Requires Python3.

## Instructions
All scripts executed from the base directory.

Execute `./setup.sh` once. Installs dependencies, sets up Python venv, and so on.

Execute `./run.sh` to start Flask webserver. Allows local testing before pushing a change.

Execute `./build.sh` to build static copy of site in `build` folder. Not so useful in itself.

Execute `./deploy.sh` to generate static files and push them to the kevinpgalligan.github.io repository.
