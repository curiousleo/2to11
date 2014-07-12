-- library/NegamaxAI.hs
-- | A negamax AI player for 2048.
module NegamaxAI (
      negamaxAI
    , utility
    ) where

import Prelude hiding (mapM)

import Data.Function (on)
import Data.List (maximumBy)
import Data.Tree (Tree (..), unfoldForest)
import qualified Data.Vector as V

import AI
import Game

type Heuristic = Board -> Int
type GameTree = Tree GameNode
data GameNode = GameNode { lastMove :: Move, state :: Board }

data Move = ComputerMove Position Value
          | PlayerMove Direction

-- | Generate an infinite game forest.
gameTree :: Board       -- ^ Start state
         -> [GameTree]  -- ^ Forest of game trees
gameTree = unfoldForest f . expandPlayer' where
    f node@(GameNode mv board) = (,) node $ case mv of
        (PlayerMove _)     -> expandComputer' board
        (ComputerMove _ _) -> expandPlayer' board
    expandComputer' board =
        [ computerNode board pos val | pos <- freePositions board, val <- [1, 2] ]
    expandPlayer' board =
        [ playerNode board dir | dir <- possibleMoves board ]
    computerNode board pos val =
        GameNode (ComputerMove pos val) $ place pos val board
    playerNode board dir =
        GameNode (PlayerMove dir) $ fst $ move dir (board, 0)

-- | Cut a tree at a certain depth.
cutTree :: Int      -- ^ Depth
        -> Tree a   -- ^ Original (possibly infinite) tree
        -> Tree a   -- ^ Finite tree of given depth
cutTree 0 (Node label _)        = Node label []
cutTree n (Node label children) = Node label $ map (cutTree (n-1)) children

-- | Negamax AI.
negamaxAI :: Heuristic  -- ^ Utility function to use
          -> Int        -- ^ Depth of the negamax search
          -> AI         -- ^ Resulting AI player
negamaxAI util depth = direction . maximumBy (compare `on` negamax util) . map (cutTree depth) . gameTree where
    direction (Node (GameNode (PlayerMove dir) _) _) = dir

-- | Negamax algorithm.
negamax :: Heuristic    -- ^ Heuristic to use
        -> GameTree     -- ^ (Finite) game tree
        -> Int          -- ^ Heuristic of the game tree
negamax util (Node node []      ) = util $ state node
negamax util (Node node children) = let nextLevel = map (negamax util) children in
    case lastMove node of
        (PlayerMove _)     -> maximum nextLevel
        (ComputerMove _ _) -> minimum nextLevel

utility :: Heuristic
utility = numEmpty

-- | The number of empty fields on the board.
numEmpty :: Heuristic
numEmpty = V.foldl (flip $ (+) . numEmpty') 0 . unBoard where
    numEmpty' = V.foldl (flip $ \x -> ifZero x succ id) 0
    ifZero 0 x _ = x
    ifZero _ _ y = y
