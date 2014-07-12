-- library/NegamaxAI.hs
-- | A negamax AI player for 2048.
module NegamaxAI (
      negamaxAI
    , utility
    ) where

import Prelude hiding (mapM)

import Control.Arrow (second)

import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Vector as V

import AI
import Game

type Player = Board -> [Board]
type Heuristic = Board -> Int

negamaxAI :: Heuristic  -- ^ Utility function to use
          -> Int        -- ^ Depth of the negamax search
          -> AI         -- ^ Resulting AI player
negamaxAI util depth = fst . maximumBy (compare `on` snd) . map (second negamax') . expandPlayer where
    negamax' = negamax depth (expandComputer, map snd . expandPlayer) util

negamax :: Int              -- ^ Depth
        -> (Player, Player) -- ^ Player and opponent
        -> Heuristic        -- ^ Heuristic to use
        -> Heuristic        -- ^ Negamax itself is a heuristic
negamax 0 _        util state = util state
negamax d (pl, op) util state = maximum ((:) minBound $ map negamax' $ pl state) where
    negamax' :: Heuristic
    negamax' = negamax (d-1) (op, pl) (negate . util)

expandComputer :: Board -> [Board]
expandComputer board =
    [ place pos val board | pos <- freePositions board, val <- [1, 2] ]

expandPlayer :: Board -> [(Direction, Board)]
expandPlayer board =
    [ (dir, fst $ move dir (board, 0)) | dir <- possibleMoves board ]

utility :: Heuristic
utility = numEmpty

numEmpty :: Heuristic
numEmpty = V.foldl (flip $ (+) . numEmpty') 0 . unBoard where
    numEmpty' = V.foldl (flip $ \x -> ifZero x succ id) 0
    ifZero 0 x _ = x
    ifZero _ _ y = y
