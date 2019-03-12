#!/bin/bash

find src -name '*.hs' | xargs graphmod -q | dot -Tpng -Gdpi=300 -o src.png
find test -name '*.hs' | xargs graphmod -q -p | dot -Tpng -Gdpi=300 -o test.png
