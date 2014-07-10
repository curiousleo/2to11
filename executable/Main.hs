module Main (main) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (runMaybeT)

import Game
import AI

main :: IO ()
main = do
    init <- runMaybeT $ playComputer <=< playComputer $ (emptyBoard (5, 5), 0)
    print $ init
