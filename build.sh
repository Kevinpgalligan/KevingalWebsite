#!/usr/bin/env bash
source env/bin/activate
python3 app.py "${@:1}" && cp -r ./straight-copy-files/* ./build/
