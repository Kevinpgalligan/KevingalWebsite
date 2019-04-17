#!/usr/bin/env bash

function pushWithConfirmation {
    cd "$1"
    if [[ -z "$(git status --porcelain)" ]];
    then
        echo -e "\e[91mNo changes made.\e[0m"
        return
    fi
    git add .
    git commit -m "$(date)"
    git diff HEAD^ HEAD
    read -p "Happy with diff? (y/n) " shouldPush
    if [[ -z "${shouldPush}" ]] || [[ "${shouldPush}" != "y" ]];
    then
        echo -e "\e[91mAbandoning push.\e[0m"
        git reset --hard HEAD^
    else
        printf "Pushing.\n"
        git push
        printf "Pushed.\n"
    fi
}

function deleteOldWebsiteFilesWithConfirmation {
    cd "$1"
    echo "Deleting old website files."
    echo "Here is the rm command, executed from directory $(pwd):"
    echo rm -r $(ls .)
    read -p "Happy with the command? (y/n) " shouldDelete
    if [[ -z "${shouldDelete}" ]] || [[ "${shouldDelete}" != "y" ]];
    then
        echo "Cancelling delete operation."
        return 1
    else
        echo "Carrying out delete operation."
        rm -r $(ls "$1")
        return 0
    fi
}

if [[ -z "$1" ]] || [[ "$1" != *kevinpgalligan.github.io/ ]];
then
    echo -e "\e[91mWebsite repository path must be passed.\e[0m"
    exit 1
fi

./build.sh
(deleteOldWebsiteFilesWithConfirmation "$1")
if [[ $? -ne 0 ]];
then
    echo -e "\e[91mDeletion of old files failed, abandoning deployment.\e[0m"
    exit 1
fi
cp -r ./build/* "$1"
(pushWithConfirmation "$1")

