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

-- | A linear version of something Generic could derive
toRepr :: [a] ->. Either () (a, [a])
toRepr [] = Left ()
toRepr (a:l) = Right (a, l)

fromRepr :: Either () (a, [a]) ->. [a]
fromRepr (Left ()) = []
fromRepr (Right (a, l)) = a:l

instance Traversable [] where
  traverse _ [] = Linear.pure []
  traverse f (a:l) = (:) Linear.<$> f a Linear.<*> traverse f l

  traverseP p =
    Profunctor.dimap toRepr fromRepr $
    Profunctor.second (p Profunctor.*** traverseP p)
