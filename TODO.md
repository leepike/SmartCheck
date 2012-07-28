TODO
-----------------------------------------------

* Make sure I can use extrapolation on its own, without reduce.

* Testing with arguments to value constructors omitted in the SubTypes
  instances.

* Right now, in DataToTree, sub (and functions it depends on) fail silently if
  you give them an index that is out of bounds.  Perhaps I should return a
  Maybe?  

* Options:
  * Option for controlling depth into structure we recurse.

* Performance analysis of extrapolation.

* Have extrapolate check whether we've run out of sub constructors and let the
  user know.

* Lists are base types??

* Document SubTypes class methods!

* We extrapolate if there exists at least one test that satisfies the
  precondition, and for all tests that asatisfy the precondition, they fail.  Is
  this the right thing to do?  (I think it is.)

* Args to optionally extrapolate and contructor extrapolate.

* Make sure that foldM (extractResult prop) FailedPreCond res in SmartGen fails
  eagerly.

* Add option to bound depth of reducing.

* Need to do constructor gen for *all* previously-found counterexamples, so we
get their constructors, too.

* Web view for large data (like Yav's thing for type nats)?

* Would I benefit from using a zipper for traversal?  I suspect not since I am
  really don't a fold. (Oleg Kiselyov, 27 Apr 2005, haskell\@, "Zipper as a delimited continuation")

Won't Do / Can't Do
-----------------------------------------------
* Use shrink instances as default for base types.

  * It's not clear there's a benefit to this.  What's difficult is not
    understanding some base-type failure but the structure of large data.  This
    seems like wasted time.  In any event, there's commented out code in
    smartShrink (in Reduce) that will do this.

* Make sure that printing, etc. doesn't depend on subTypes.  Just want that to
  do with replacing values during testing.  I'm not sure I can do this
  independently of SubT.

  * I think I need something like this.

* I don't think I can make a generic instance for the arbitrary method.  This is
  because I don't take a value and apply a function to it.  Rather, I want to
  generate a new value.  But Generics expects to have some sort of
  representation of the data you're manipulating.

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
