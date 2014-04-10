#!/bin/bash

g++ $1 -o 2runx
./2runx
RETX=$?
rm 2runx
exit $RETX
