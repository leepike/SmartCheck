# QuickCheck implementation notes for my own use.  (The design is rather clever. :) I *think* this is how things work...)

---------------------------------------

The main entry point is `quickCheckWithResult`.  There, a property (something that
belongs to the `Testable` class) is turned into a `Property`:

    type Property = Gen Prop
    newtype Prop = MkProp{ unProp :: Rose Result }
    data Rose a = MkRose a [Rose a] | IORose (IO (Rose a))

(more on that in a bit) by using `Testable`'s `property` method.  At
this point, it's important to know that we have everything we need to test some
function: we've generated random values for the function's inputs, and we have a
tree of results based on shrinking the values in the Rose tree.  So none of the
data structures (e.g., Result) need to be paramaterized by the function argument
types.

So `quickCheckWithResult` calls `test`, which calls `runATest`.  There, we loop
(by calling `continue`, locally defined in `runATest`) if we don't get a
failure.  If we do get a failure, we start shrinking in the call

    foundFailure st res ts

`foundFailure` starts a loop of looking through the Rose Tree to find smaller
failing values (`localMin'` calls `foundFailure` to complete the loop).

The real magic is in the `Testable` class in the Rose data structure, both
defined in the `Property` module.  Starting with `Testable`: basically, you're
going to have some function `f :: X -> Y -> ... -> Bool` that you want to test.
QuickCheck knows how to make arbitrary values for `X`, `Y`, etc.  So the first
instance you'll probably encounter is

    instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
      property f = forAllShrink arbitrary shrink f

`forallShrink` calls `shrinking`, which actually makes the `Property`.

    shrinking :: Testable prop =>
                 (a -> [a])  -- ^ 'shrink'-like function.
              -> a           -- ^ The original argument
              -> (a -> prop) -> Property
    shrinking shrinker x0 pf = fmap (MkProp . joinRose . fmap unProp) (promote (props x0))
     where props x = MkRose (property (pf x)) [ props x' | x' <- shrinker x ]

We "tie the knot" by calling `property (pf x)` here, where as you'll recall, is
the method of the `Testable` class.  So if you have more arguments to the
function, they'll get consumed.  Recall for each argument for the function,
we'll call `forAllShrink arbitrary shrink f` again...  So here,

    props :: a -> Rose Property
      ===> alpha-renames to
    props :: a -> Rose (Gen Prop)

which is a little confusing.  This is a rose tree of Rose `Result`s: each
element is a rose tree!  But `joinRose` combines the "inner" Rose tree with the
outer one, so we're left with a sane rose tree.

I guess the way to think about the type `Rose Result` is that for a value

    MkRose v ls

`v` is the result of evaluating the function, and `ls` holds all possible
shrinks for `v`.  If `v` fails, we want to traverse down `ls`, finding the best
shrinking.  The nondeterminism held by the Rose tree is to keep us from favoring
one argument's shrink over another.  That is, for

    foo :: Int -> Int -> Bool
    foo x y = ...

we don't want to favor the shrinking results of `x` over `y`.  The Rose tree
gives us a list of shrinks with `x`, and from these, a list of shrinks with `y`.
So if the first shrink of `x` doesn't work, we'll ignore all `y`s there.

You can see this in the definition of `noShrinking`:

    noShrinking :: Testable prop => prop -> Property
    noShrinking = mapRoseResult (onRose (\res _ -> MkRose res []))

It throws away the shrinking values.

When testing a function like

    prop :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
    prop a b c d e f g h i j = average [a, b, c, d, e, f, g, h, i, j] < 1000
      where
      average [] = 0
      average ls = fromIntegral (sum ls) / fromIntegral (length ls)

QuickCheck doesn't seem to favor (overly) early arguments over late ones.  Of
course, you're always just finding a local minimum...  I don't think there's
backtracking.

Where did the argument types go in all of this?  The `Result` (or `State`) data
structures don't carry any information about the argument types.  This all goes
away in `forAllShrink` (and `shrinking`), after which we have a `Property` data
structure.  Note, for example, in the call to `counterexample` in `forAllShrink`
there is an explicit use of `show`.

