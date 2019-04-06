#!/usr/bin/env bash
# Sets up virtual env from which to run Python.
# Must be executed from the home directory.
# Works with Python3.
python3 -m pip install --user virtualenv
python3 -m virtualenv env
source env/bin/activate
pip3 install -r requirements.txt