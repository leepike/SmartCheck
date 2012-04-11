TODO
-----------------------------------------------
* Rename Reduce to SmartShrink

* Use [GHC generics](http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-programming.html)
  instead of SubTypes class.

* Make it an argument how many extrapolation tests you do.


Not sure how to do
-----------------------------------------------
* Replace (a -> Bool) properties with Testable properties.
  
  * Not quite sure how to do this...

* Use shrink instances as default for base types.
  
  * I don't think this works.  This requires knowing that a field also has an
    instance for Arbitrary.  I have no guarantee of that.


Done
-----------------------------------------------
* ~~Rename examples/Test to examples/MutRecData~~

  * ~~Rename it in the README, too.~~
