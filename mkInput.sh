#!/bin/bash
i=0
cat streamly-ghc9-regression.cabal > input.txt
while test $i -le 15
do
  cat input.txt > input1.txt
  cat input.txt >> input1.txt
  mv input1.txt input.txt
  i=$(($i+1))
done
