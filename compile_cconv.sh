#!/bin/sh

# COMPILING: Compile a file to closure-converted ocaml and call ocamlopt
# to produce the binary. Closure conversion is typed, using OCaml objects.
# The goal here is to run the output through the OCaml type checker.

if [ X"$1" != X"-o" -o X"$2" = X"" -o X"$3" = X"" ]; then
   echo "./usage: ./compile_cconv -o output file"
   exit 1
fi


./camlc.byte -dcconv "$3" >  camlc_out.ml &&
# The reason for this bizarre construct is that -dcconv does not include
# Cl_camldefs.
cat cl_camldefs.ml camlc_out.ml > __out.ml &&
mv __out.ml camlc_out.ml &&
ocamlc -g -o "$2" cl_camldefs.ml camlc_out.ml



