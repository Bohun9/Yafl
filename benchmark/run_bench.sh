#!/bin/bash

echo "========================"
echo "Haskell..."
ghc fib.hs -o fib_hs > /dev/null 2>&1 && time ./fib_hs

echo "========================"
echo "Haskell optimized..."
ghc fib.hs -O2 -o fib_hs > /dev/null 2>&1 && time ./fib_hs

echo "========================"
echo "Ocaml..."
ocamlopt fib.ml -o fib_ml > /dev/null 2>&1 && time ./fib_ml

echo "========================"
echo "Ocaml optimized..."
ocamlopt -O2 fib.ml -o fib_ml > /dev/null 2>&1 && time ./fib_ml

echo "========================"
echo "Yafl..."
(cd .. && cabal run yafl -- benchmark/fib.yafl -o benchmark/fib_yafl > /dev/null 2>&1) && time ./fib_yafl

echo "========================"
echo "Yafl optimized..."
(cd .. && cabal run yafl -- benchmark/fib.yafl -o benchmark/fib_yafl -O > /dev/null 2>&1) && time ./fib_yafl
