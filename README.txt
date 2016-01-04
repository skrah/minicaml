
MiniCaml is a compiler for a very restricted subset of Caml with an LLVM
backend. The feature set is very similar to Appel's Tiger language. Obviously
this compiler is educational in nature.

Highlights:
===========

   o Typed closure conversion (see e.g. "Typed Closure Conversion" by Minamide,
     Morrisett, Harper). Optionally the output of the conversion can be dumped
     and compiled as valid OCaml (using objects as closures).

   o LLVM backend. The closure conversion uses static links and should
     be grossly inefficient. The Boehm garbage collector is used.
     All values are boxed using a struct of two pointers.  Despite all
     this LLVM cleans up pretty well.


Drawbacks:
==========

   o No variant types: The compiler still needs nil.

   o No documentation (read the shell scripts to start with).

   o No real error messages in the type-checking phase.

   o This is really an alpha version. Expect breakage.


License:
========

   o Q-Public License. Modifications must be distributed in the form of patches.


Author:
=======

   Stefan Krah <stefan@bytereef.org>


