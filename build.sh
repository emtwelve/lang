#!/bin/bash
c2hs Lang.chs
ghc -c -g -debug -O Lang.hs
opts="--make -no-hs-main -g -debug -optc -g -optc -I/usr/include/python3.8 -optc-O lang.c Lang"
ghc $opts -o ffilang 2>/dev/null
ghc $opts -optl -lpython3.8 -optl -L/usr/bin/python3.8-config -o ffilang 
#ghci -no-hs-main -optc-O lang.c Lang
rm *.o *_stub.h *.chi *.hi
