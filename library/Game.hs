-- library/Game.hs
-- | The game logic for 2048.
module Game (
      Direction (..)
    , Board (..)
    , Position
    , State
    , Value

    , emptyBoard

    , playComputer

    , canMove
    , possibleMoves

    , dimensions

    , free
    , freePositions

    , move
    , place
    ) where

import Control.Arrow (first)
import Control.Monad (mzero, guard)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Class (lift)

import qualified Data.Vector as V
import System.Random (getStdRandom, randomR)

-- | Models the 2048 game board as a vector of int vectors.
newtype Board = Board { unBoard :: V.Vector (V.Vector Value) }
    deriving Eq
type Value = Int

-- | A position on the game board.
type Position = (Int, Int)

-- | The state, modelled as the game board and the current score.
type State = (Board, Int)

-- | A player's move.
data Direction = R | U | L | D
    deriving (Show, Eq)

-- | Player swipes right, up, left, or down.
move :: Direction   -- ^ The direction of the move
     -> State       -- ^ The current state
     -> State       -- ^ The next state
move R = move'
move U = first rot90  . move' . first rot270
move L = first rot180 . move' . first rot180
move D = first rot270 . move' . first rot90

-- | Computer places a 2 or 4 on a free space.
place :: Position   -- ^ The position of the newly placed number
      -> Value      -- ^ What value to place
      -> Board      -- ^ The current state
      -> Board      -- ^ The next state
place (r, c) val = Board . place' . unBoard where
    place' vec = vec V.// [(r, vec V.! r V.// [(c, val)])]

playComputer :: State -> MaybeT IO State
playComputer (board, score) = do
    board' <- placeRandom board
    guard $ not . null . possibleMoves $ board'
    return (board', score)

placeRandom :: Board -> MaybeT IO Board
placeRandom board = do
    pos <- choose $ freePositions board
    val <- choose $ 2 : replicate 9 1
    return $ place pos val board

choose :: [a] -> MaybeT IO a
choose [] = mzero
choose xs = do
    let range = (0, length xs - 1)
    i <- lift $ getStdRandom $ randomR range
    return $ xs !! i

-- | Can the player move in the given direction?
canMove :: Direction -> Board -> Bool
canMove dir board = 0 /= score || board /= board' where
    (board', score) = move dir (board, 0)

-- | Which moves are possible?
possibleMoves :: Board -> [Direction]
possibleMoves board = filter (flip canMove board) [R, U, L, D]

-- | Can the computer put a new number at the given position?
free :: Position -> Board -> Bool
free (r, c) = (== 0) . (\vec -> (vec V.! r) V.! c) . unBoard

-- | Which positions are still free?
freePositions :: Board -> [Position]
freePositions board = filter (flip free board) positions where
    positions = [ (i,j) | i <- [0 .. r-1], j <- [0 .. c-1] ]
    (r, c) = dimensions board

move' :: State -> State
move' (board, score) = (Board vec', score + V.sum scores) where
    (_, w) = dimensions board
    combined = V.map (first (restore w) . compact) (unBoard board)
    (vec', scores) = V.unzip combined

rot90, rot180, rot270 :: Board -> Board
rot90 board = Board rotated where
    rotated = V.generate w (\i -> V.generate h (\j -> rot90' i j))
    rot90' i j = ((unBoard board) V.! j) V.! (w-i-1)
    (h, w) = dimensions board
rot180 = rot90 . rot90
rot270 = rot90 . rot90 . rot90

compact :: V.Vector Value -> (V.Vector Value, Value)
compact = first (V.fromList . dropWhile (== 0)) . V.foldr f ([], 0) where
    f 0 acc         = acc
    f y ([], s)     = ([y], s)
    f y ((0:xs), s) = ((y:xs), s)
    f y (l@(x:xs), s)
        | x == y    = ((0:x':xs), s')
        | otherwise = ((y:l), s)
        where
            x' = succ x
            s' = s + 2^x'

restore :: Int -> V.Vector Value -> V.Vector Value
restore n vec = V.replicate (n - V.length vec) 0 V.++ vec

dimensions :: Board -> (Int, Int)
dimensions board
    | V.null vec = (0, 0)
    | otherwise  = (V.length vec, V.length (vec V.! 0))
    where
        vec = unBoard board

example :: Board
example = Board $ V.fromList (map V.fromList m) where
    m = [[0, 1, 0, 1, 2],
         [0, 1, 1, 2, 3],
         [1, 1, 2, 2, 2]]

emptyBoard :: Position -> Board
emptyBoard (r, c) = Board $ V.replicate r $ V.replicate c 0

instance Show Board where
    show = showBoard

showBoard :: Board -> String
showBoard = V.foldl (\s -> \v -> s ++ ('\n' : (showRow v))) "" . unBoard where
    showRow = concat . V.toList . V.map showNum
    showNum 0 = replicate 5 ' ' ++ "0"
    showNum x = let s = show ((2::Value)^x) in (replicate (6 - length s) ' ') ++ s
