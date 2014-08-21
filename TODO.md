# TODO

* Die on exceptions from QuickCheck.

* Testing with arguments to value constructors omitted in the SubTypes
  instances.

* Web view for large data (like Yav's thing for type nats)?

* I would (maybe?) benefit from using a zipper for traversal.

* Refactor so we only getAtIdx (which is expensive!) once per pass.

* Pass around stdGen so that the code is more pure.

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
