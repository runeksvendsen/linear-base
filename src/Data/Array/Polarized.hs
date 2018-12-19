{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Data.Array.Polarized where

import Data.Array.Destination (DArray)
import qualified Data.Array.Destination as DArray
import qualified Data.Functor.Linear as Data
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude.Linear

-- See:
--
-- - http://lopezjuan.com/limestone/vectorcomp.pdf
-- - http://jyp.github.io/posts/controlled-fusion.html

data PushArray a where
  PushArray :: (forall b. (a ->. b) -> DArray b ->. ()) ->. Int -> PushArray a
  -- A note on implementation: `exists b. ((a -> b), DArray b)` adjoins freely
  -- the structure of contravariant functor to `DArray`. Because it appears to
  -- the left of an arrow, we can curry the existential quantification (and,
  -- less crucially, the pair) so that we can spare an extra type definition.
  --
  -- An invariant is kept that, really, the first parameter is only past arrays
  -- of size @n@ (the second parameter, which is the length of the array).

instance Data.Functor PushArray where
  fmap f (PushArray k n) = PushArray (\g dest -> k (g . f) dest) n

-- TODO: Vector should be unrestricted here.
-- XXX: the use of Vector in the type of alloc is temporary (see also "Data.Array.Destination")
alloc :: PushArray a ->. Vector a
alloc (PushArray k n) = DArray.alloc n (k id)

-- Is it preferable that a pushArray be only used linearly? If so should it be enforced in the type of walk?
walk :: Vector a -> PushArray a
walk as =
  PushArray
    (\g dest -> DArray.mirror as g dest)
    (Vector.length as)

-- TODO: linear semigroup/monoid type classes
append :: PushArray a ->. PushArray a ->. PushArray a
append (PushArray kl nl) (PushArray kr nr) =
    PushArray
      (\f dest -> parallelApply f kl kr (DArray.split nl dest))
      (nl+nr)
  where
    parallelApply :: (a ->. b) -> ((a ->. b) -> DArray b ->. ()) ->. ((a ->. b) -> DArray b ->. ()) ->. (DArray b, DArray b) ->. ()
    parallelApply f' kl' kr' (dl, dr) = kl' f' dl `lseq` kr' f' dr
