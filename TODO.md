TODO
-----------------------------------------------

* Make sure I can use extrapolation on its own, without reduce.

* Testing with arguments to value constructors omitted in the SubTypes
  instances.

* Right now, in DataToTree, sub (and functions it depends on) fail silently if
  you give them an index that is out of bounds.  Perhaps I should return a
  Maybe?  

* Have extrapolate check whether we've run out of sub constructors and let the
  user know.

* Document SubTypes class methods!

* Args to optionally extrapolate and contructor extrapolate.

* Need to do constructor gen for *all* previously-found counterexamples, so we
get their constructors, too.

* Web view for large data (like Yav's thing for type nats)?

* I would benefit from using a zipper for traversal.

* Try out SubTypes instances for Map a b and try an algebraic type inside of
  lists (maybe Forest Foo).

* Go through Reddit comments: (in refs/)

* Try to fix/simplify definition of showForest.

* Probably, list/map [a] should be a baseType if a is a baseType and not
  otherwise.

* Include instance for Data.Map in SubType instances?  Argument for not:
  QuickCheck doesn't include them as basic instances...

* Test () SubType method.

* Refactor so we only getAtIdx (which is expensive!) once per pass.

* Pass around stdGen so that the code is more pure.

* Bug with XMonad stuff.  Look in examples/StackSet.  There you'll see that
  getAtIdx does not get the same value as replaceAtIdx.  I'm working on
  narrowing the bug in StackSetMinBug.  

Won't Do / Can't Do
-----------------------------------------------
* Use shrink instances as default for base types.

  * It's not clear there's a benefit to this.  What's difficult is not
    understanding some base-type failure but the structure of large data.  This
    seems like wasted time.  In any event, there's commented out code in
    smartShrink (in Reduce) that will do this.

* I don't think I can make a generic instance for the arbitrary method.  This is
  because I don't take a value and apply a function to it.  Rather, I want to
  generate a new value.  But Generics expects to have some sort of
  representation of the data you're manipulating.

* QuickCheck uses a clever trick with typeclasses that allows them to generate
  and test functions in the [Property
  module](http://hackage.haskell.org/packages/archive/QuickCheck/2.5/doc/html/src/Test-QuickCheck-Property.html#exhaustive).
  I was thinking it might be nice to follow their approach with SmartCheck, but
  there are a few problems/lack of motivation:

  * SmartCheck addresses the problem of getting complex counterexamples.
    Usually, we imagine there's one complex datatype, and maybe some Ints,
    Chars, etc. that we also want to randomly generate and test.  In this case,
    it makes sense to focus on the one.

  * With QuickCheck, there are essentially two passes: (1) make some arbitrary
    values and test them, and (2) shrink the values.

Done
-----------------------------------------------
* ~~Rename examples/Test to examples/MutRecData~~

  * ~~Rename it in the README, too.~~

* ~~Replace (a -> Bool) properties with Properties.  This is needed in case we
  originally want to omit certain values from triggering a failure.~~

* ~~Collect failed tests as preconditions until we reach a fixed point.~~

* ~~When I'm shrinking, if I find a hole that is of the same type (via cast)
  that also fails the property, replace the original value with the hole.~~

* Options:
  * ~~Show output as data-tree or just replace values in extrapolated value.~~

* ~~In strForest, I need to remove parens when removing subterms.  "()" could be
  Units!.  Probably just bite the bullet and use parsec.~~
