module Main (main) where

import AI

main :: IO ()
main = do
    let n = 1000
    let dims = (4, 4)
    scores <- mapM (play dims) $ replicate n baselineAI
    print $ stats scores

stats :: [Int] -> (Int, Double, Int)
stats xs = (mini, avg, maxi) where
    mini = minimum xs
    avg = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)
    maxi = maximum xs
