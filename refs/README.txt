-- Refs ------------------------------------------------------------------------

* Feat in Haskell '12: http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/Testing-Feat

* The SYB paper:
  http://research.microsoft.com/en-us/um/people/simonpj/Papers/hmap/index.htm
  Has shrink example

* The Scrap your zippers paper:
  https://www.cs.indiana.edu/~adamsmd/papers/scrap_your_zippers/ 
  (I could build on this)

* QuickSpec: 
  http://www.cse.chalmers.se/~nicsma/quickspec.pdf

* http://blog.regehr.org/archives/697 --- check out C-reduce (Regehr) and Delta
  debugging (http://www.st.cs.uni-saarland.de/dd/).  C-reduce is in PLDI.

* Also check out http://www.whyprogramsfail.com/

* GenCheck https://github.com/JacquesCarette/GenCheck

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

> Properties.mytest Properties.prop_findIndex 1000

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
