#!/usr/bin/env bash
source env/bin/activate
args=build
if [ $# -ge 0 ]; then
	args="build ${1}"
fi
python3 app.py ${args} && cp -r ./straight-copy-files/* ./build/
