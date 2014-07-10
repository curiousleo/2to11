module Main (main) where

import Control.Monad ((<=<), liftM)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import Game
import AI

play :: (State -> MaybeT IO State) -> IO State
play ai = last `liftM` do
    (Just start) <- runMaybeT initial
    unfoldr' (runMaybeT . next ai) start

next :: (State -> MaybeT IO State) -> State -> MaybeT IO State
next ai s = (playComputer <=< ai) =<< (MaybeT . return . return $ s)

initial :: MaybeT IO State
initial = playComputer <=< playComputer $ (emptyBoard (5, 5), 0)

main :: IO ()
main = do
    let n = 100
    scores <- mapM play $ replicate n playAI'
    print $ (fromIntegral . sum . snd . unzip $ scores) / (fromIntegral n)

unfoldr' :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
unfoldr' f x = do
    maybe_x' <- f x
    case maybe_x' of
        Just x' -> do
            l <- unfoldr' f x'
            return $ x' : l
        Nothing ->
            return []
