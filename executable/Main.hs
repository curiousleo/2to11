module Main (main) where

import AI
import NegamaxAI

main :: IO ()
main = do
    let n = 100
    let dims = (4, 4)
    scores <- mapM (play dims) $ replicate n $ negamaxAI utility 0
    print $ stats scores

stats :: [Int] -> (Int, Double, Int)
stats xs = (mini, avg, maxi) where
    mini = minimum xs
    avg = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)
    maxi = maximum xs
