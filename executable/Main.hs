module Main (main) where

import Control.Monad ((<=<), liftM)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import Game
import AI

play :: (GameState -> MaybeT IO GameState) -> IO GameState
play ai = last `liftM` do
    (Just start) <- runMaybeT initial
    unfoldr' (runMaybeT . next ai) start

next :: (GameState -> MaybeT IO GameState) -> GameState -> MaybeT IO GameState
next ai s = (playComputer <=< ai) =<< (MaybeT . return . return $ s)

initial :: MaybeT IO GameState
initial = playComputer <=< playComputer $ (emptyBoard (4, 4), 0)

main :: IO ()
main = do
    let n = 1000
    scores <- mapM play $ replicate n (playAiT baselineAi)
    print $ stats (snd . unzip $ scores)

stats :: [Int] -> (Int, Double, Int)
stats xs = (mini, avg, maxi) where
    mini = minimum xs
    avg = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)
    maxi = maximum xs

unfoldr' :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
unfoldr' f x = do
    maybe_x' <- f x
    case maybe_x' of
        Just x' -> do
            l <- unfoldr' f x'
            return $ x' : l
        Nothing ->
            return []
