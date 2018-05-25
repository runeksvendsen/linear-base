{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
import Control.Monad (void)
import qualified Data.List as L
import Data.Typeable
import qualified Foreign.Heap as Heap
import Foreign.List (List)
import qualified Foreign.List as List
import Foreign.Marshal.Pure (Pool)
import qualified Foreign.Marshal.Pure as Manual
import Prelude (return)
import Prelude.Linear
import Test.Hspec
import Test.QuickCheck

eqList :: forall a. (Manual.Representable a, Movable a, Eq a) => List a ->. List a ->. Unrestricted Bool
eqList l1 l2 =
    eqUL (move (List.toList l1)) (move (List.toList l2))
  where
    eqUL :: Unrestricted [a] ->. Unrestricted [a] ->. Unrestricted Bool
    eqUL (Unrestricted as1) (Unrestricted as2) = Unrestricted (as1 == as2)

data InjectedError = InjectedError
  deriving (Typeable, Show)

instance Exception InjectedError

main :: IO ()
main = hspec $ do
  describe "Off-heap lists" $ do
    describe "ofList" $ do
      it "is invertible" $
        property (\(l :: [Int]) -> unUnrestricted (Manual.withPool $ \pool ->
          let
            check :: Unrestricted [Int] ->. Unrestricted Bool
            check (Unrestricted l') = Unrestricted $ l' == l
          in
            check $ move (List.toList $ List.ofList l pool)))

    describe "map" $ do
      it "of identity if the identity" $
        property (\(l :: [Int]) -> unUnrestricted (Manual.withPool $ \pool ->
          let
            check :: (Pool, Pool, Pool) ->. Unrestricted Bool
            check (pool1, pool2, pool3) =
              eqList
                (List.map (\x -> x) (List.ofList l pool1) pool2)
                (List.ofList l pool3)
          in
            check (dup3 pool)))

    -- XXX: improve the memory corruption test by adding a 'take n' for a random
    -- 'n' before producing an error.
    describe "exceptions" $ do
      it "doesn't corrupt memory" $ do
        property (\(l :: [Int]) -> do
          let l' = l ++ (throw InjectedError)
          catch @InjectedError
            (void $ evaluate
               (Manual.withPool $ \pool ->
                   move (List.toList $ List.ofRList l' pool)))
            (\ _ -> return ())
           )


  describe "Off-heap heaps" $ do
    describe "sort" $ do
      it "sorts" $
        property (\(l :: [(Int, ())]) -> Heap.sort l == (L.reverse $ L.sort l))
