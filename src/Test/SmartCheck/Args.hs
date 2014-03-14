-- | SmartCheck arguments.

module Test.SmartCheck.Args
  ( ScArgs(..)
  , scStdArgs
  , Format(..)
  ) where

import qualified Test.QuickCheck as Q

-------------------------------------------------------------------------------

data Format = PrintTree | PrintString
  deriving (Eq, Read, Show)

data ScArgs =
  ScArgs { format       :: Format    -- ^ How to show extrapolated formula
                                     --------------
         , qcArgs       :: Q.Args    -- ^ QuickCheck arguments
                                     --------------
         , scMaxSize    :: Int       -- ^ Maximum size of data to generate, in
                                     --   terms of the size parameter of
                                     --   QuickCheck's Arbitrary instance for
                                     --   your data.
                                     --------------
         , scMaxDepth   :: Maybe Int -- ^ How many levels into the structure of
                                     --   the failed value should we descend
                                     --   when reducing or generalizing?
                                     --   Nothing means we go down to base
                                     --   types.
                                     --------------
         -- Reduction
         , scMaxReduce  :: Int       -- ^ How hard (number of rounds) to look
                                     --   for failure in the reduction stage.
                                     --------------
         -- Extrapolation
         , runForall    :: Bool      -- ^ Should we extrapolate?
                                     --------------
         , scMaxForall  :: Int       -- ^ How hard (number of rounds) to look
                                     --   for failures during the extrapolation
                                     --   stage.
                                     --------------
         , scMinForall  :: Int       -- ^ Minimum number of times a property's
                                     -- precondition must be passed to
                                     -- generalize it.
                                     --------------
         -- Constructor generalization
         , runExists    :: Bool      -- ^ Should we try to generalize
                                     --   constructors?
                                     --------------
         , scMaxExists  :: Int     -- ^ How hard (number of rounds) to look
                                     -- for failing values with each
                                     -- constructor.  For "wide" sum types, this
                                     -- value should be increased.
                                     --------------
         } deriving (Show, Read)

--------------------------------------------------------------------------------

scStdArgs :: ScArgs
scStdArgs = ScArgs { format       = PrintTree
                   , qcArgs       = Q.stdArgs
                   , scMaxSize    = 10
                   , scMaxDepth   = Nothing
                   ---------------------
                   , scMaxReduce  = 100
                   ---------------------
                   , runForall    = True
                   , scMaxForall  = 20
                   , scMinForall  = 10
                   ---------------------
                   , runExists    = True
                   , scMaxExists  = 20
                   }

--------------------------------------------------------------------------------
