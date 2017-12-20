{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Prelude.Linear
  ( -- * Standard 'Prelude' function with linear types
    -- $linearized-prelude
    ($)
  , const
  , swap
  , seq
    -- * Unrestricted
    -- $ unrestricted
  , Unrestricted(..)
  , unUnrestricted
    -- * Typeclasses for non-linear actions
    -- $ comonoid
  , Consumable(..)
  , Dupable(..)
  , Movable(..)
  , lseq
    -- * Linear monad hierarchy
    -- $ monad
  , LFunctor(..)
  , LApplicative(..)
  , LMonad(..)
  , lreturn
  , ljoin
    -- * Re-exports from the standard 'Prelude' for convenience
  , module Prelude
  ) where

import qualified Unsafe.Linear as Unsafe
import Prelude hiding
  ( ($)
  , const
  , seq
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  )
import qualified Prelude

-- $linearized-prelude

-- A note on implementation: to avoid silly mistakes, very easy functions are
-- simply reimplemented here. For harder function, we reuse the Prelude
-- definition and make an unsafe cast.

-- | Beware: @($)@ is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
($) :: (a ->. b) ->. a ->. b
-- XXX: Temporary as `($)` should get its typing rule directly from the type
-- inference mechanism.
($) f x = f x

infixr 0 $

const :: a ->. b -> a
const x _ = x

-- XXX: To be decided: In `base`, this is not a prelude function (it's in
-- `Data.Tuple`), maybe we don't want it to be in `Prelude.Linear`.
swap :: (a,b) ->. (b,a)
swap (x,y) = (y,x)

-- | @seq x y@ only forces @x@ to head normal form, therefore is not guaranteed
-- to consume @x@ when the resulting computation is consumed. Therefore, @seq@
-- cannot be linear in it's first argument.
seq :: a -> b ->. b
seq x = Unsafe.toLinear (Prelude.seq x)

-- $ unrestricted

-- | @Unrestricted a@ represents unrestricted values of type @a@ in a linear context,
data Unrestricted a where
  Unrestricted :: a -> Unrestricted a

-- | Project an @a@ out of an @Unrestricted a@. If the @Unrestricted a@ is
-- linear, then we get only a linear value out.
unUnrestricted :: Unrestricted a ->. a
unUnrestricted (Unrestricted a) = a

-- $ comonoid

class Consumable a where
  consume :: a ->. ()

-- | Like 'seq' but since the first argument is restricted to be of type @()@ it
-- is consumed, hence @seqUnit@ is linear in its first argument.
seqUnit :: () ->. b ->. b
seqUnit () b = b

-- | Like 'seq' but the first argument is restricted to be 'Consumable'. Hence the
-- first argument is 'consume'-ed and the result consumed.
lseq :: Consumable a => a ->. b ->. b
lseq a b = seqUnit (consume a) b

-- | The laws of @Dupable@ are dual to those of 'Monoid':
--
-- * @first consume (dup a) ≃ a ≃ first consume (dup a)@ (neutrality)
-- * @first dup (dup a) ≃ (second dup (dup a))@ (associativity)
--
-- Where the @(≃)@ sign represent equality up to type isomorphism
class Consumable a => Dupable a where
  dup :: a ->. (a, a)

-- | The laws of the @Movable@ class mean that @move@ is compatible with @consume@
-- and @dup@.
--
-- * @case move x of {Unrestricted _ -> ()} = consume x@ (this law is trivial)
-- * @case move x of {Unrestricted x -> x} = x@
-- * @case move x of {Unrestricted x -> (x, x)} = dup x@
class Dupable a => Movable a where
  move :: a ->. Unrestricted a

instance Consumable () where
  consume () = ()

instance Dupable () where
  dup () = ((), ())

instance Movable () where
  move () = Unrestricted ()

instance Consumable Bool where
  consume True = ()
  consume False = ()

instance Dupable Bool where
  dup True = (True, True)
  dup False = (False, False)

instance Movable Bool where
  move True = Unrestricted True
  move False = Unrestricted False

-- TODO: instances for Int, primitive tuples

instance Consumable a => Consumable [a] where
  consume [] = ()
  consume (a:l) = consume a `lseq` consume l

instance Dupable a => Dupable [a] where
  dup [] = ([], [])
  dup (a:l) = shuffle (dup a) (dup l)
    where
      shuffle :: (a, a) ->. ([a], [a]) ->. ([a], [a])
      shuffle (a', a'') (l', l'') = (a':l', a'':l'')

instance Movable a => Movable [a] where
  move [] = Unrestricted []
  move (a:l) = liftu (move a) (move l)
    where
       -- XXX: this is merely an application of 'Unrestricted' being a linear
       -- applicative functor of some sort.
      liftu :: Unrestricted a ->. Unrestricted [a] ->. Unrestricted [a]
      liftu (Unrestricted a') (Unrestricted l') = Unrestricted (a':l')

instance Consumable (Unrestricted a) where
  consume (Unrestricted _) = ()

instance Dupable (Unrestricted a) where
  dup (Unrestricted a) = (Unrestricted a, Unrestricted a)

instance Movable (Unrestricted a) where
  move (Unrestricted a) = Unrestricted (Unrestricted a)

-- $monad

-- XXX: even if the monad hierarchy is in the Prelude. This should probably be
-- moved somewhere else in order to use namespace rather than the rather
-- invasive `l` prefix in front of everything (plus, that would make infix
-- symbols possible again)

-- TODO: explain that the category of linear function is self-enriched, and that
-- this is a hierarchy of enriched monads. In order to have some common vocabulary.

-- There is also room for another type of functor where map has type `(a ->.b)
-- -> f a ->. f b`. `[]` and `Maybe` are such functors (they are regular
-- (endo)functors of the category of linear functions whereas `LFunctor` are
-- enriched functors). A Traversable hierarchy would start with non-enriched
-- functors.

-- TODO: make the laws explicit

-- | Enriched linear functors.
class LFunctor f where
  lfmap :: (a ->. b) ->. f a ->. f b

-- | Enriched linear applicative functors
class LFunctor f => LApplicative f where
  lpure :: a ->. f a
  lap :: f (a ->. b) ->. f a ->. f b

-- | Enriched linear monads
class LApplicative m => LMonad m where
  lbind :: m a ->. (a ->. m b) ->. m b
  lsc :: m () ->. m a ->. m a

-- | Handles pattern-matching failure in do-notation. See 'Control.Monad.Fail'.
class LMonad m => LMonadFail m where
  lfail :: String -> m a

{-# INLINE lreturn #-}
lreturn :: LMonad m => a ->. m a
lreturn x = lpure x

ljoin :: LMonad m => m (m a) ->. m a
ljoin action = action `lbind` (\x -> x)

-- | Type of 'monadBuilder'. Note how the constraint on @m@ varies depending on
-- the field. The constraints are solved lazily when a field is used by the do
-- notation (in particular, if you don't do a pattern-matching, then you don't
-- need a 'LMonadFail').
data BuilderType = Builder
  { (>>=) :: forall m a b. LMonad m => m a ->. (a ->. m b) ->. m b
  , (>>) :: forall m b. LMonad m => m () ->. m b ->. m b
  , fail :: forall m a. LMonadFail m => String -> m a
  }

-- | A builder to be used with @-XRebindableSyntax@ in conjunction with
-- @RecordWildCards@
monadBuilder :: BuilderType
monadBuilder = Builder { (>>=) = lbind, (>>) = lsc, fail = lfail }
