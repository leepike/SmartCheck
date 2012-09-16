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

* Dart-check: http://code.google.com/p/dart-immutable/ (and G+ post by Paul
  Brauner)

* Feat: Functional Enumeration of Algebraic Types: http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/Testing-Feat

-- notes -----------------------------------------------------------------------

We'd love to just say that we can't further reduce some hole in a datatype if at
the hole, we have a depth of 0.  However, consider the following. In d0, the
hole (D0 (H I)) has three constructors.  But there is no possible smaller value
to put in here.  So we just have to test if we can make something smaller.

We could do a static analysis to figure this out, if we can get the arity of
constructors at compile-time.

data I = I
  deriving (Show, Read, Typeable, Generic)

instance SubTypes I

instance Arbitrary I where
  arbitrary = undefined

data H = H I
  deriving (Show, Read, Typeable, Generic)

instance SubTypes H

instance Arbitrary H where
  arbitrary = undefined

data D = D0 H
       | D1 D 
       | D2 D D
--       | D3 String
  deriving (Show, Read, Typeable, Generic)

instance SubTypes D

instance Arbitrary D where
  arbitrary = undefined

d0 = D2 (D0 (H I)) (D1 (D1 (D0 (H I))))

samples generates values based on arbitrary, using sized to control size.
This may have no relation to how many constructors there are in the depth of
the value, although it'll likely strongly coorelate with it.  There are a few
options: (1) Ask the user to ensure that the definition of the arbitrary
instance satisfies our invariant.  (2) Try to make an infinite (or
sufficiently long) list of values in ascending size and dynamicly ensure we
take a value sufficiently small.  (3) Try to overapproximate the size and
still do a dynamic check.

(1) Is very hard for the user to check.  (2) is also difficult, because the
very definition of arbitrary randomly generates values in a range of sizes.
So we really want to sort this list, which can only be done if it's finite.
(3) is an approximate solution insofar as it requires some computation we'd
like to avoid.

Finally, it'd be good to generate values lazily, to avoid generating
intermediate values.

What I really want is a bijection from a set of sized value to values, and a
function taking a value, and telling me what sized was used.


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

