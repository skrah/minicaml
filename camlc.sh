#!/bin/sh

# COMPILING: Compile a file to LLVM IR and call clang to produce the binary.

if [ X"$1" != X"-o" -o X"$2" = X"" -o X"$3" = X"" ]; then
   echo "./usage: ./camlc -o output file"
   exit 1
fi

./camlc.byte -emit-llvm "$3" 2> __out.ll &&
clang -Wall -W -O3 -o "$2" __out.ll runtime.c -lgc



