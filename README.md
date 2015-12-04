# Build Instructions

Requirements (others may work, but they are untested):

- GHC version           >= 7.6.2
- cabal-install version >= 1.18.0.2
- Cabal library version >= 1.18.1.2

The build system assumes a cabal-sandbox build.

    > make configure
    > make all

# Synopsis

SmartCheck is a smarter [QuickCheck](http://hackage.haskell.org/package/QuickCheck), a powerful testing library for Haskell.  The purpose of SmartCheck is to help you more quickly get to the heart of a bug and to quickly discover _each_ possible way that a property may fail.

SmartCheck is useful for debugging programs operating on algebraic datatypes.  When a property is true, SmartCheck is just like QuickCheck (SmartCheck uses QuickCheck as a backend).  When a property fails, SmartCheck kicks into gear.  First, it attempts to find a _minimal_ counterexample to the property in a robust, systematic way.  (You do not need to define any custom shrink instances, like with QuickCheck, but if you do, those are used.  SmartCheck usually can do much better than even custom shrink instances.)  Second, once a minimal counterexample is found, SmartCheck then attempts to generalize the failed value `d` by replacing `d`'s substructures with new values to make `d'`, and QuickChecking each new `d'`.  If for each new `d'` generated, the property also fails, we claim the property fails for any substructure replaced here (of course, this is true modulo the coverage of the tests).

SmartCheck executes in a read-eval-print loop.  In each iteration, all values that have the "same shape" as the generalized value are removed from possible created tests.  The loop can be iterated until a fixed-point is reached, and SmartCheck is not able to create any new values that fail the property.

# A typical example

In the package there is an examples directory containing a number of examples.  Let's look at the simplest, [Div0.hs](https://github.com/leepike/SmartCheck/blob/master/examples/Div0.hs).

    > cd SmartCheck/examples
    > ghci -Wall Div0.hs

Div0 defines a toy language containing constants (C), addition (A), and division (D):

    data M = C Int
           | A M M
           | D M M
      deriving (Show, Typeable, Generic)

Because SmartCheck performs data-generic operations using GHC.Generics we have to derive Typeable and Generic.  To use GHC.Generics, you also need the following pragmas: and the single automatically-derived instance:

    {-# LANGUAGE DeriveDataTypeable #-}
    {-# LANGUAGE DeriveGeneric #-}

    instance SubTypes M

Let's say we have a little interpreter for the language that takes care to return `Nothing` if there is a division by 0:

    eval :: M -> Maybe Int
    eval (C i) = Just i
    eval (A a b) = do
      i <- eval a
      j <- eval b
      return $ i + j
    eval (D a b) =
      if eval b == Just 0 then Nothing
        else do i <- eval a
                j <- eval b
                return $ i `div` j

Now suppose we define a set of values of M such that they won't result in division by 0.  We might try the following:

    divSubTerms :: M -> Bool
    divSubTerms (C _)       = True
    divSubTerms (D _ (C 0)) = False
    divSubTerms (A m0 m1)   = divSubTerms m0 && divSubTerms m1
    divSubTerms (D m0 m1)   = divSubTerms m0 && divSubTerms m1

So our property (tries) to state that so long as a value satisfies divSubTerms, then we won't have division by 0 (can you spot the problem in `divSubTerms`?):

    div_prop :: M -> Property
    div_prop m = divSubTerms m ==> eval m /= Nothing

Assuming we've defined an Arbitrary instance for M (just like in
QuickCheck---however, we just have to implement the arbitrary method; the shrink
method is superfluous), we are ready to run SmartCheck.  The property you want
to check must be an instance of the type

    prop :: Testable a => a -> b

where `Testable` is defined by QuickCheck, and `a` is the datatype you want to
reduce (I've left off some class constraints for `a` taken care of above).  So
while we can test properties with an arbitrary number of arguments, the *first*
argument is the one we assume you want to reduce (remember your friend, `flip`!).

In this example, we won't redefine any of QuickCheck's standard arguments, but it's certainly possible.  the treeShow field tells SmartCheck whether you want generalized counterexamples shown in a tree format or printed as a long string (the default is the tree format).

    divTest :: IO ()
    divTest = smartCheck args div_prop
      where
      args = scStdArgs { qcArgs   = stdArgs
                       , treeShow = PrintString }

Ok, let's try it.  First, SmartCheck just runs QuickCheck:

    *Div0> divTest
    *** Failed! Falsifiable (after 7 tests):
    D (D (D (A (C (-20)) (D (D (C 2) (C (-19))) (C (-8)))) (D (D (C (-23)) (C 32)) (C (-7)))) (A (A (C 2) (C 10)) (A (C (-2)) (C 13)))) (D (A (C 12) (C (-7))) (D (A (C (-29)) (C 19)) (C 30)))

Oh, that's confusing, and for such a simple property and small datatype!  SmartCheck takes the output from QuickCheck and tries systematic shrinking for the one failed test-case, kind of like [SmallCheck](http://www.cs.york.ac.uk/fp/smallcheck/) might.  We get the following reduced counterexample:

    *** Smart Shrinking ...
    *** Smart-shrunk value:
    D (C 0) (D (C 0) (C (-1)))

Ok, that's some progress!  Now SmartCheck attempts to generalize this (local) minimal counterexample.  SmartCheck has two generalization steps that we'll explain separately although SmartCheck combines their results in practice (you can turn off each kind of generalization in the flags).  First, SmartCheck tries to generalize *values* in the shrunk counterexample.  SmartCheck returns

    *** Extrapolating values ...
    *** Extrapolated value:
    forall x0:

    D x0 (D (C 0) (C (-1)))

Ahah!  We see that for any possible subvalues x0, the above value fails.  Our precondition divSubTerms did not account for the possibility of a non-terminal divisor evaluating to 0; we only pattern-matched on constants.

In addition, SmartCheck tries to do something I call *constructor generalization*.  For a datatype with a finite number of constructors, the idea is to see if for each subvalue in the counterexample, there are subvalues that also fail the property using every possible constructor in the datatype.  So for example, for our counterexample above

    *** Extrapolating constructors ...
    *** Extrapolated value:
    forall C0:
      there exist arguments s.t.

    D (C 0) (D C0 (C (-1)))

So in the hole `C0`, SmartCheck was able to build a value using each of the constructors `C`, `A`, and `D` (well, it already knew there was a value using `C`---`C 0`).

SmartCheck asks us if we want to continue:

    Attempt to find a new counterexample? ('Enter' to continue; any character
    then 'Enter' to quit.)

SmartCheck will omit any term that has the "same shape" as `D (C 0) (D (C 0) (C (-1)))` and try to find a new counterexample.

    *** Failed! Falsifiable (after 9 tests):
    A (A (D (C (-20)) (A (C (-5)) (C (-32)))) (D (A (C 6) (C 19)) (A (C (-3)) (A (C (-16)) (C (-13)))))) (D (C 29) (D (C (-11)) (D (C 11) (C 23))))

    *** Smart Shrinking ...
    *** Smart-shrunk value:
    A (C (-1)) (D (A (C 1) (C 1)) (D (C 1) (C 2)))

    *** Extrapolating values ...

    *** Extrapolating Constructors ...

    *** Extrapolated value:
    forall values x0 x1:

    A x1 (D x0 (D (C 1) (C 2)))

We find another counterexample; this time, the main constructor is addition.

We might ask SmartCheck to find another counterexample:

    ...

    *** Extrapolating ...
    *** Could not extrapolate a new value; done.

At this point, SmartCheck can't find a newly-shaped counterexample.  (This doesn't mean there aren't more---you can try to increase the standard arguments to QuickCheck to allow more failed test before giving up (maxDiscard) or increasing the size of tests (maxSize).  Or you could simply just keep running the real-eval-print loop.)

# Other notes

- More details can be found in the [technical paper](paper/paper.pdf).

- We use
  [GHC Generics](http://www.haskell.org/ghc/docs/7.4.1/html/libraries/ghc-prim-0.2.0.0/GHC-Generics.html).
  You may have to define new instances in [src/Test/SmartCheck/Types.hs](src/Test/SmartCheck/Types.hs) .  Email (leepike at Gmail)
  (or submit patches) if you need instances for other types.
