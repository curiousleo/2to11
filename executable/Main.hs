module Main (main) where

import AI

main :: IO ()
main = do
    let n = 1000
    let ev ai = runEval (4,4) ai eval
    scores <- mapM ev $ replicate n baselineAI
    print $ stats scores

stats :: [Int] -> (Int, Double, Int)
stats xs = (mini, avg, maxi) where
    mini = minimum xs
    avg = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)
    maxi = maximum xs
