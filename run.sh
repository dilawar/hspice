#!/bin/bash
set +e
make clean
bnfc -m -haskell -d Jeera.cf
make 
./Jeera/Test < example.cir
