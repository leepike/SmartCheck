TODO
-----------------------------------------------
* Rename Reduce to SmartShrink

* Use [GHC generics](http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-programming.html)
  instead of SubTypes class.

* When I'm shrinking, if I find a hole that is of the same type (via cast) that
  also fails the property, replace the original value with the hole.

* Use instances so I can pass anything that can be turned into a property to
  reduce, like in QuickCheck.

* Make sure I can use extrapolation on its own, without reduce.

* Testing with arguments to value constructors omitted in the SubTypes
  instances.

* Right now, in DataToTree, sub (and functions it depends on) fail silently if
  you give them an index that is out of bounds.  Perhaps I should return a
  Maybe?  


Won't Do / Can't Do
-----------------------------------------------
* Use shrink instances as default for base types.

  * It's not clear there's a benefit to this.  What's difficult is not
    understanding some base-type failure but the structure of large data.  This
    seems like wasted time.  In any event, there's commented out code in
    smartShrink (in Reduce) that will do this.


Done
-----------------------------------------------
* ~~Rename examples/Test to examples/MutRecData~~

  * ~~Rename it in the README, too.~~

* ~~Replace (a -> Bool) properties with Properties.  This is needed in case we
  originally want to omit certain values from triggering a failure.~~

* ~~Collect failed tests as preconditions until we reach a fixed point.~~

