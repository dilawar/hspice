#!/bin/bash
set -e
make clean
bnfc -m -haskell -d Jeera.cf
if [[ $1 ]]; then
    ghc Jeera.hs -o jeera
    ./jeera < example.cir
fi
