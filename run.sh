#!/bin/bash
set +e
make clean
bnfc -m -haskell -d Jeera.cf
if [[ $1 ]]; then
    make 
    ./Jeera/Test < example.cir
fi
