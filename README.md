Synopsis
--------------------------------

SmartCheck is a smarter
[QuickCheck](http://hackage.haskell.org/package/QuickCheck), a powerful testing
library for Haskell.  The purpose of SmartCheck is to help you more quickly get
to the heart of a bug and to quickly all the ways that a property fails.

SmartCheck is useful for debugging programs operating on algebraic datatypes.
When a property is true, SmartCheck is just like QuickCheck (SmartCheck uses
QuickCheck as a backend).  When a fails, QuickCheck does two things.  First, it
attempts to find a minimal counterexample to the property is a robust,
systematic way.  (You do not need to define any custom shrink instances, like
with QuickCheck.)  Second, once a minimal counterexample is found, SmartCheck
then attempts to generalize the failed value v by replacing the v's
substructures with new values to make v', and QuickChecking each new v'.  If for
each new v' generated, the property also fails, we claim the property fails for
any substructure replaced here (of course, this is true modulo the coverage of
the tests).

SmartCheck executes in a real-eval-print loop.  In each iteration, all values
that have the "same shape" as the generalized value is removed from possible
created tests.  The loop can be iterated until a fixed-point is reached, and
SmartCheck is not able to create any new values that fail the property.

A typical example
--------------------------------

In the package there is an examples directory containing a number of examples.
Let's look at the simplest,
[Div0.hs](https://github.com/leepike/SmartCheck/blob/master/examples/Div0.hs).

    > cd SmartCheck/examples
    > ghci -Wall Div0.hs

Div0 defines a toy language containing constants (C), addition (A), and division
(D):

    data M = C Int
           | A M M
           | D M M
      deriving (Read, Show, Data, Typeable, Generic)

Because SmartCheck performs data-generic operations using Data.Data and
GHC.Generics we have to derive Data, Typeable, and Generic (we plan to eliminate
dependence on Data.Data in the future).  To use GHC.Generics, you also need the
following pragmas: and the single automatically-derived instance:

    {-# LANGUAGE DeriveDataTypeable #-}
    {-# LANGUAGE DeriveGeneric #-}

    instance SubTypes M 

Let's say we have a little interpreter for the language that takes care not to
divide by 0:

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

Now suppose we define a set of values in M such that they won't result in
division by 0.  We might try the following:

    divSubTerms :: M -> Bool
    divSubTerms (C _)       = True
    divSubTerms (D _ (C 0)) = False
    divSubTerms (A m0 m1)   = divSubTerms m0 && divSubTerms m1
    divSubTerms (D m0 m1)   = divSubTerms m0 && divSubTerms m1

(Can you spot the problem?)

So our property states that so long as a value satisfies divSubTerms, then we
won't have division by 0:

div_prop :: M -> Property
div_prop m = divSubTerms m ==> eval m /= Nothing

Assuming we've defined an Arbitrary instance for M (just like in
QuickCheck---however, we just have to implement the arbitrary method; the shrink
method is superfluous), we are ready to run SmartCheck.

divTest :: IO ()
divTest = smartCheck args div_prop
  where 
  args = scStdArgs { qcArgs   = stdArgs 
                   , treeShow = PrntString }

In this example, we won't redefine any of QuickCheck's standard arguments, but
it's certainly possible.  the treeShow field tells SmartCheck whether you want
generalized counterexamples shown in a tree format or printed as a long string
(the default is the tree format).

Ok, let's try it.  First, SmartCheck just runs QuickCheck:

    *Div0> divTest 
    *** Failed! Falsifiable (after 11 tests):  
    A (A (D (A (C (-3)) (C 40)) (C 11)) (D (C (-5)) (C (-42)))) (D (A (D (C (-9)) (C 29)) (C (-23))) (A (D (C (-6)) (D (C 23) (C (-20)))) (D (D (C (-13)) (C (-35))) (D (C 0) (C (-4))))))

It then takes the output from QuickCheck and tries systematic shrinking:

    *** Smart Shrinking ... 
    *** Smart-shrunk value:
    D (D (C (-13)) (C (-35))) (D (C 0) (C (-4)))

Ok, that's some progress!  Now SmartCheck attempt to generalize this minimal counterexample:

    *** Extrapolating ...
    *** Extrapolated value:
    forall x0 x1:

    D x1 (D (C 0) x0)

Ahah!  We see that for any possible subvalues x0 and x1, the above value fails.
Our precondition divSubTerms did not account for the possibility of a
non-terminal divisor evaluating to 0; we only pattern-matched on constants.  

SmartCheck asks us if we want to continue:

    Attempt to find a new counterexample? ('Enter' to continue; any character
    then 'Enter' to quit.)

SmartCheck will omit any term that has the "same shape" as D x1 (D (C 0) x0) and
try to find a new counterexample.  

    *** Failed! Falsifiable (after 13 tests):  
    D (A (C 23) (C (-52))) (A (D (D (C 52) (C 72)) (D (D (D (C 28) (C (-32))) (A (C 36) (C (-85)))) (C (-32)))) (A (A (A (C (-122)) (C (-76))) (C 52)) (D (D (D (C 91) (C 98)) (D (C 122) (C 64))) (C 5))))

    *** Smart Shrinking ... 
    *** Smart-shrunk value:
    D (D (C 52) (C 72)) (A (C (-1)) (C 1))

    *** Extrapolating ...
    *** Extrapolated value:
    forall x0:

    D x0 (A C (-1) (C 1))

We find another counterexample; this time, the divisor is an addition term.

We might ask SmartCheck to find another counterexample: 

    ...

    *** Extrapolating ...
    *** Could not extrapolate a new value; done.

At this point, SmartCheck can't find a newly-shaped counterexample.  (This
doesn't mean there aren't more---you can try to increase the standard arguments
to QuickCheck to allow more failed test before giving up (maxDiscard) or
increasing the size of tests (maxSize).  Or you could simply just keep running
the real-eval-print loop.
