#!/bin/bash

test () {
    echo $1
    doctest $1
}

if [ "$#" -ne 1 ]; then
    for f in *.hs; do
        test "$f"
    done
else
    test "$1"
fi
