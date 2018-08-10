{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Relative.Linear where

import GHC.Types (Multiplicity(..))

-- | This module defines a hierarchy of type classes for functors which are not,
-- mathematically, endofunctors. Their control flow is linear, but they can
-- contain values of varying multiplicities. At the top of the hierarchy is a
-- class of relative monads.

-- TODO: move to Prelude
data Mult (p :: Multiplicity) a where
  Mult :: a -->. (p) Mult p a

class Functor (f :: Multiplicity -> * -> *) where
  fmap :: (a -->. (p) Mult q b) ->. f p a ->. f q b

class Applicative (f :: Multiplicity -> * -> *) where
  pure :: a -->. (p) f p a
  -- The following type is not correct!
  (<*>) :: f r (a -->. (p) Mult q b) ->. f p a ->. f q b

class Monad (f :: Multiplicity -> * -> *) where
  (>>=) :: m p a ->. (a -->. (p) m q b) ->. m q b
  (>>) :: m p () ->. m q a ->. m q a

data StateResult s (p :: Multiplicity) a where
  SR :: a -->. (p) s ->. s

newtype State s p a = State (s ->. SR s p a)

instance Functor State where
  fmap f x = State $ \s -> cont f (x s)
    where
      cont :: (a -->.(p) Mult q b) ->. StateResult s p a -> StateResult s q b
      cont f (SR s a) = cont' s (f b)

      cont' :: s ->. Mult q b ->. StateResult s q b
      cont' s (Mult b) = SR s b

instance Applicative State where
  pure a = State $ \s -> SR s a -- TODO: i.e. flip SR
  (<*>) = error "TODO: applicative state"

instance Monad State where
  x >>= f = State $ \s -> cont f (x s)
    where
      cont :: (a -->. (p) State q b) ->. SR s p a -> SR s q b
      cont f (SR s a) = f a s
