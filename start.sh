#!/bin/sh
erl -pa ebin deps/*/ebin -s signals \
    -eval "io:format(\"Signals started~n\")."
