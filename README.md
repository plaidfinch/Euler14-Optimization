Euler14-optimization
====================

Progressively optimized versions (in Haskell) of a solution to Problem 14 from Project Euler.

Compilation Notes
-----------------

Version 4 needs extra stack space and should be compiled with `-rtsopts -with-rtsopts -K50M` (for default problem size), and possibly with larger sizes than `50M` in case of larger problem sizes.

Version 6 is multithreaded and must be compiled as such. Use: `-threaded -rtsopts -withrtsopts -N8`. For best performance on modern processors with hyperthreading, substitute for `8` the number of cores on your machine multiplied by two.
