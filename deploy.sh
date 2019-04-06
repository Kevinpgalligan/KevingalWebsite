#!/usr/bin/env bash

# Pushes copy of static site to remote master.
# You should consider testing the changes before running this
# script, otherwise you might break the site.

./build.sh
rm -r /tmp/website-build
mkdir /tmp/website-build
mv build/ /tmp/website-build/
git stash --include-untracked

# We'll use this later to remove annoying files from the commit on the master branch.
# Exclude build/, the static site files are in there.
ignorefiles=$(cat .gitignore | sed ':a;N;$!ba;s/\n/ /g')

git checkout master
git rm -r *
mv /tmp/website-build/* .
git add .

# Files in .gitignore are brought over to the master branch. We don't want to keep
# a copy of .gitignore on the master branch, since it's only supposed to store static
# files for the website. So here we remove them all the junk before committing.
for file in "${ignorefiles}"; do git reset HEAD ${file}; done

git commit -m "$(date)"
git push
git checkout dev
git stash apply
