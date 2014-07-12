-- library/NegamaxAI.hs
-- | A negamax AI player for 2048.
module NegamaxAI (
      Color (..)

    , cutTree

    , expandPlayer
    , expandComputer

    , negamaxAI

    , utility

    , gameTree
    ) where

import Prelude hiding (mapM)

import Control.Arrow (second)

import Data.Function (on)
import Data.List (maximumBy)
import Data.Tree (Tree (..), unfoldTree)
import qualified Data.Vector as V

import AI
import Game

type Player = Board -> [Board]
type Heuristic = Board -> Int
data Color = Max | Min deriving (Eq, Show)
type GameTree = Tree GameNode
type GameNode = (Color, Board)

colorise :: Color -> Int -> Int
colorise Max = id
colorise Min = negate

swapColor :: Color -> Color
swapColor Max = Min
swapColor Min = Max

gameTree :: Color -> (Player, Player) -> Board -> GameTree
gameTree color (maxP, minP) board = unfoldTree f (color, board) where
    f node@(c, b) = (node, [ (swapColor c, b') | b' <- moves c b ])
    moves Max = maxP
    moves Min = minP

cutTree :: Int -> Tree a -> Tree a
cutTree 0 (Node label _)        = Node label []
cutTree n (Node label children) = Node label $ map (cutTree (n-1)) children

negamaxAI :: Heuristic  -- ^ Utility function to use
          -> Int        -- ^ Depth of the negamax search
          -> AI         -- ^ Resulting AI player
negamaxAI util depth = fst . maxSnd . map (second negamax') . expandPlayer where
    maxSnd = maximumBy (compare `on` snd)
    minTree = gameTree Min (expandComputer, map snd . expandPlayer)
    negamax' = negamax util . cutTree depth . minTree

negamax :: Heuristic        -- ^ Heuristic to use
        -> GameTree         -- ^ (Finite) game tree
        -> Int
negamax util (Node (color, board) [])       = colorise color $ util board
negamax util (Node _              children) = maximum $ map negamax' $ children
    where
        negamax' = negate . negamax util

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
