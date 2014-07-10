module GameSpec (spec) where

import qualified Data.Vector as V

import Game
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, vector)

instance Arbitrary Board where
    arbitrary = fmap Board $ fmap (gen l) (vector (l*l)) where
        gen l xs = V.generate l (\i -> V.generate l (\j -> (xs !! j*5 + i)))
        l = 5

spec :: Spec
spec = do
    describe "rot90" $ do
        prop "is the identity when applied four times" $
            \x -> (rot90 . rot90 . rot90 . rot90) x == (x :: Board)
