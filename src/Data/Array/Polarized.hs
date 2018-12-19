{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Data.Array.Polarized where

import Data.Array.Destination (DArray)
import qualified Data.Array.Destination as DArray
import Prelude.Linear
import qualified Data.Functor.Linear as Data

data PushArray a where
  PushArray :: (forall r b. (a ->. b) -> DArray b ->. r) ->. Int -> PushArray a
  -- A note on implementation: `exists b. ((a -> b), DArray b)` adjoins freely
  -- the structure of contravariant functor to `DArray`. Because it appears to
  -- the left of an arrow, we can curry the existential quantification (and,
  -- less crucially, the pair) so that we can spare an extra type definition.
  --
  -- An invariant is kept that, really, the first parameter is only past arrays
  -- of size @n@ (the second parameter, which is the length of the array).

instance Data.Functor PushArray where
  fmap f (PushArray k n) = PushArray (\g dest -> k (g . f) dest) n

-- TODO: linear semigroup/monoid type classes
append :: PushArray a ->. PushArray a ->. PushArray a
append (PushArray kl nl) (PushArray kr nr) =
    PushArray
      (\f dest -> parallelApply f kl kr (DArray.split nl dest))
      (nl+nr)
  where
    parallelApply :: (a ->. b) -> ((a ->. b) -> DArray b ->. r) ->. ((a ->. b) -> DArray b ->. r) ->. (DArray b, DArray b) ->. r
    parallelApply f' kl' kr' (dl, dr) = (kl' f' dl, kr' f' dr)
