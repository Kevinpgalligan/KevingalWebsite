#!/usr/bin/env bash

if [[ -z "$1" ]]
then
    echo -e "\e[91mWebsite repository path must be passed."
    exit 1
fi

./build.sh
cp -r ./build/* "$1"
(cd "$1"; git add .; git commit -m "$(date)"; git diff HEAD^ HEAD; git push)
