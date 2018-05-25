{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Data.Traversable.Linear
  ( -- * Linear traversable hierarchy
    -- $ traversable
    Functor(..)
  , Traversable(..)
  ) where

import qualified Control.Monad.Linear as Linear
import Data.Profunctor.Linear (Profunctor)
import qualified Data.Profunctor.Linear as Profunctor
import Data.Void
import Prelude.Linear

-- $traversable

-- TODO: write the laws
-- TODO: maybe add a Foldable class between Functor and Traversable as well

-- | Linear functors
class Functor f where
  fmap :: (a ->. b) -> f a ->. f b

-- | Enriched linear applicative functors
class Functor t => Traversable t where
  {-# MINIMAL traverseP, traverse | traverseP, sequenceA #-}

  traverseP ::
    ( Profunctor.Strong (,) () arr
    , Profunctor.Strong Either Void arr
    , Profunctor.Monoidal (,) () arr)
    => (a `arr` b) -> (t a `arr` t b)

  traverse :: Linear.Applicative f => (a ->. f b) -> t a ->. f (t b)
  {-# INLINE traverse #-}
  traverse f = sequenceA . fmap f

  sequenceA :: Linear.Applicative f => t (f a) -> f (t a)
  {-# INLINE sequenceA #-}
  sequenceA = traverse id

  mapM :: Linear.Monad m => (a ->. m b) -> t a ->. m (t b)
  {-# INLINE mapM #-}
  mapM = traverse

  sequence :: Linear.Monad m => t (m a) -> m (t a)
  {-# INLINE sequence #-}
  sequence = sequenceA

-- Helpers for traverseP.
-- TODO: find a good place

-- | To understand 'Traversed', let us think about the standard traversal for
-- list, whose main case is defined as @traverse f (x:l) = (:) <$> f x <*>
-- traverse f l@. What is going to happen is during the traversals, two
-- different sort of things will be pushed to the stack: the @f a@, and the
-- constructors. A value of 'Traversed' if of the form @Next a1 (Next a2 … (Next
-- an (NoMore (\b1 b2 … bn -> u))) … )@ where the @ai@ represent the @f x@,
-- while the closure in the final @NoMore@ represents the constructors (or
-- rather, it represents the expression with holes @(:) <$> _ <*> traverse f l@,
-- that is the constructor, and how it is applied). Therefore, a value of type
-- 'Traversed a b t' can be seen as the reification of a traversal.
data Traversed a b t
  = NoMore t
  | Next a (Traversed a b (b ->. t))

toRepr :: Traversed a b t ->. Either t (a, Traversed a b (b ->. t))
toRepr (NoMore t) = Left t
toRepr (Next a k) = Right (a, k)

ofRepr :: Either t (a, Traversed a b (b ->. t)) ->. Traversed a b t
ofRepr (Left t) = NoMore t
ofRepr (Right (a, k)) = Next a k

-- instance Functor (Traversed a b) where

emit :: a ->. Traversed a b b
emit a = Next a (NoMore id)

traverseToTraversed :: Traversable t => t a -> Traversed a b (t b)
traverseToTraversed x = traverse emit x

traversePTraversed ::
  ( Profunctor.Strong Either Void arr
  , Profunctor.Monoidal (,) () arr)
  => (a `arr` b) -> (Traversed a c t `arr` Traversed b c t)
traversePTraversed f = Profunctor.dimap toRepr ofRepr $
  Profunctor.second $
  f Profunctor.*** traversePTraversed f

-- TODO: when polymorphic flip is available, implement in terms of flip
forM :: (Linear.Monad m, Traversable t) => t a ->. (a ->. m b) -> m (t b)
forM cont act = mapM act cont

-- TODO: We can drop the @Consumable (t ())@ constraint if we make
-- @traverse_@ a member of the 'Traverse' type class. This is probably desirable
-- | Note that the linear 'forM_' has a @'Consumable' (t ())@ constraint: @const
-- ()@ is not linear, therefore to be able to deduce 'forM_' from the
-- 'Traversable' instance, we need to consume the resulting container (which is
-- not free).
forM_
  :: (Linear.Monad m, Traversable t, Consumable (t ()))
  => t a ->. (a ->. m ()) -> m ()
forM_ cont act = Linear.void $ forM cont act


------------------------
-- Standard instances --
------------------------

instance Functor [] where
  fmap _ [] = []
  fmap f (a:l) = (f a):(fmap f l)

-- instance Traversable [] where
--   traverse _ [] = Linear.pure []
--   traverse f (a:l) = (:) Linear.<$> f a Linear.<*> traverse f l

--   traverseP p =
--     -- Profunctor.dimap toRepr fromRepr $
--     -- Profunctor.second (p Profunctor.*** traverseP p)
--     traverse _
