-- xmonad ----------------------------------------------------------------------

* internals overview:
  http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Developing.html

* hackage: http://hackage.haskell.org/package/xmonad

* properties are inside 0.10/xmonad-0.10/tests/

INSTRUCTIONS

* Blow away your cabal-dev directories to do a fresh install.

Make latest smartcheck: from SmartCheck:
> cabal-dev install

Make smartcheck available: from xmonad-0.10/
> cabal-dev add-source ../../../../

Turn on testing:
> cabal-dev install -ftesting

From xmonad-0.10/
> cabal-dev ghci -Wall 

> Properties.main 

or to test individual props:

> :m + Test.QuickCheck.Test

> quickCheckWithResult stdArgs Properties.prop_shift_reversible

or 

> :m + Test.SmartCheck
> smartCheck scStdArgs prop_shift_reversible

(This is a good candidate for shrinking...)

* Mark changed properties or functions in StackSet.hs with -- BAD
* Changes for SmartCheck marked with -- SC

-- TODOs -----------------------------------------------------------------------

* Find disjunctive invariants---actually, just add as new preconditions to
  tests.

* Run smartcheck and quickcheck in parallel.  Maybe?

  * Run as many tests in parallel as possible.  Maybe?

* Explore the idea of data coverage:

  * E.g., trivial "false" property to see if your program exercises all
    constructors.

  * Kinds of type coverage: exists some contructor to fail...
