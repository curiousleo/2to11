module GameSpec (spec) where

import qualified Data.Vector as V

import Game
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary (arbitrary, Arbitrary)

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

spec :: Spec
spec = do
    describe "transpose" $ do
        prop "is its own reverse" $
            \x -> (transpose . transpose) x == (x :: Field)
