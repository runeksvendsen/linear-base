{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Array.Destination where

import Control.Exception (evaluate)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import GHC.Exts (RealWorld)
import qualified Prelude as Prelude
import Prelude.Linear
import System.IO.Unsafe
import qualified Unsafe.Linear as Unsafe

-- | An array destination ('DArray') is a form a constructor for arrays, which
-- make it possible to create by adding a new element to it in
-- O(1).
--
-- Destination-passing style is common in C programming, where we pass a mutable
-- data-structure to a function, whose responsibility it is to fill the array.
--
-- Through linear types, however, we can expose array destinations as /pure/
-- data structures (even though, operationally, filling the destination is
-- implemented by mutations under the hood). Moreover, we make sure that each
-- cell in the destination will be filled exactly once (in particular no cell
-- will be forgotten), and we can't read cells in the destination, therefore
-- there is no risk of reading an uninitialised cell.
newtype DArray a = DArray (MVector RealWorld a)

-- XXX: use of Vector in types is temporary. I will probably move away from
-- vectors and implement most stuff in terms of Array# and MutableArray#
-- eventually, anyway. This would allow to move the MutableArray logic to linear
-- IO, possibly, and segregate the unsafe casts to the Linear IO module.
alloc :: Int -> (DArray a ->. ()) ->. Vector a
alloc n = Unsafe.toLinear unsafeAlloc
  where
    unsafeAlloc :: (DArray a ->. ()) -> Vector a
    unsafeAlloc build = unsafeDupablePerformIO Prelude.$ do
      dest <- MVector.unsafeNew n
      evaluate (build (DArray dest))
      Vector.unsafeFreeze dest

replicate :: a -> DArray a ->. ()
replicate a = Unsafe.toLinear unsafeReplicate
  where
    unsafeReplicate (DArray ds) = unsafeDupablePerformIO Prelude.$ do
      temp <- MVector.replicate (MVector.length ds) a
      -- Note that it is indeed linear to compute the length of a MVector and
      -- keep it unchanged. Therefore, syntax apart, it is true that this
      -- function is linear.
      MVector.unsafeCopy ds temp
    -- XXX: this allocation is unnecessary, it's just a good short-cut for an
    -- initial implementation.

-- | Caution, @'fill' a dest@ will fail is @dest@ isn't of length exactly one.
fill :: a ->. DArray a ->. ()
fill = Unsafe.toLinear2 unsafeFill
    -- XXX: we will probably be able to spare this unsafe cast given a (linear)
    -- length function on destination.
  where
    unsafeFill a (DArray ds) =
      if MVector.length ds /= 1 then
        error "Destination.fill: requires a destination of size 1"
      else
        unsafeDupablePerformIO Prelude.$ MVector.write ds 0 a