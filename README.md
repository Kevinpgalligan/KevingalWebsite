## Instructions
All scripts executed from the base directory.

Execute `./setup.sh` once.

Execute `./run.sh` to start Flask webserver.

Execute `./build.sh` to build static copy of site in `build` folder.

Execute `./deploy.sh` to push static copy of site to the appropriate repository.

Requires Python3.

## Todo List
* Modify deploy script so that it deletes files that weren't generated this time. Will need an ignore list so that old files are deleted.
* Ensure that index.html gets generated.
* Content of site.
    * "Apps" page.
    * Introduction on home page.
    * At least 1 blog post.
* Style of site.
    * Nav bar needs style.
    * Something to make the page feel less "open" and scary.
* Custom 404 page(s).
* Domain name, point to site.
