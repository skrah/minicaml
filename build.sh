#!/bin/sh

ocamlbuild -use-ocamlfind -package llvm -package llvm.analysis -package ctypes camlc.byte
