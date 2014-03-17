This directory contains regression tests for SmartCheck vs. QuickCheck, Feat,
etc.

Each benchmark is in it's own directory.  The Makefile builds the binaries and
runs them.  Sometimes, we make GNUPlot plots.

We use submake here.

Test.hs is a top-level driver for doing I/O for benchmarks.
