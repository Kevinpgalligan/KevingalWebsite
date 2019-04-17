#!/usr/bin/env bash
source env/bin/activate
python3 app.py build && cp -r ./straight-copy-files/* ./build/

