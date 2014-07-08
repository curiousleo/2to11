import qualified GameSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Game" GameSpec.spec
