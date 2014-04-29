This directory contains regression tests for SmartCheck vs. QuickCheck, Feat,
etc.

Each benchmark is in it's own directory.  The Makefile builds the binaries and
runs them, using submake.  Sometimes, we make GNUPlot plots.

Test.hs is a top-level driver for doing I/O for benchmarks.
