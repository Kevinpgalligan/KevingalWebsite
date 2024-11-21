#!/usr/bin/env bash
source env/bin/activate
if [[ "${@:1}" = "" ]]; then
	echo "WARNING: You forgot to provide arguments, this will just start the server."
fi
python3 app.py "${@:1}" && cp -r ./straight-copy-files/* ./build/
