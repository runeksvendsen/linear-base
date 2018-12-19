{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

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
  --
  -- TODO: I think that changing `()` as the return type to `r` for an arbitrary
  -- (linear) `Monoid` r would also let me implement `concat :: PushArray
  -- (PushArray a) -> PushArray a`, by instantiating to `PushArray a`. Maybe.

instance Data.Functor PushArray where
  fmap f (PushArray k n) = PushArray (\g dest -> k (g . f) dest) n

-- XXX: the use of Vector in the type of alloc is temporary (see also "Data.Array.Destination")
alloc :: PushArray a ->. Vector a
alloc (PushArray k n) = DArray.alloc n (k id)

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

data PullArray a where
  PullArray :: s ->. (s ->. Maybe (s, a)) -> PullArray a
  -- This is implemented as a mere stream. Which is broadly what we want of this
  -- type, however, it is rather restrictive. I think we should be able to read
  -- out the elements in any order for this.

-- | /!\ Partial! Only works if both arrays have the same length
zipWith :: (a ->. b ->. c) -> PullArray a ->. PullArray b ->. PullArray c
zipWith f (PullArray s1 next1) (PullArray s2 next2) = PullArray (s1,s2) next
  where
    next :: (_, _) ->. Maybe _
    next (s1', s2') = analyse (next1 s1', next2 s2')

    analyse :: (Maybe _, Maybe _) ->. Maybe _
    analyse (Just (s1'', a), Just (s2'', b)) = Just ((s1'', s2''), f a b)
    analyse (Nothing, Nothing) = Nothing
    analyse o = error @_ @(_ ->. _) "Polarized.zipWith: size mismatch" o
