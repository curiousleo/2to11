-- library/Game.hs
-- | The game logic for 2048.
module Game (
      Direction (..)
    , Board (..)        -- TODO: hide constructors

    , Dimensions
    , GameState
    , Position
    , Score
    , Value

    , emptyBoard
    , initialGameState

    , playComputer

    , canMove
    , possibleMoves
    , gameOver

    , dimensions

    , free
    , freePositions

    , move
    , place
    ) where

import Control.Arrow (first)
import Control.Monad ((<=<), mzero)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)

import Data.Maybe (fromMaybe)

import qualified Data.Vector as V
import System.Random (getStdRandom, randomR)

-- | Models the 2048 game board as a vector of row vectors.
newtype Board = Board { unBoard :: V.Vector Row }
    deriving Eq

-- | A row on the board is a vector of values.
type Row = V.Vector Value

-- | The type of numbers on the board.
type Value = Int

-- | The type of the game score.
type Score = Int

-- | A position on the game board.
type Position = (Int, Int)

-- | Board dimensions
type Dimensions = Position

-- | The state, modelled as the game board and the current score.
type GameState = (Board, Score)

-- | A player's move.
data Direction = R | U | L | D
    deriving (Show, Eq)

-- | Initialise the game state by randomly placing two numbers and setting the
--   score to zero.
initialGameState :: Dimensions      -- ^ Board dimensions
                 -> IO GameState    -- ^ Initial game state
initialGameState dims = playComputer <=< playComputer $ (emptyBoard dims, 0)

-- | Player swipes right, up, left, or down. This function moves the numbers in
--   the given direction, combines them according to the game rules and records
--   the new score.
move :: Direction   -- ^ The direction of the move
     -> GameState   -- ^ The current state
     -> GameState   -- ^ The next state
move R = moveR
move U = first rot90  . moveR . first rot270
move L = first rot180 . moveR . first rot180
move D = first rot270 . moveR . first rot90

-- | Place a number on the board.
place :: Position   -- ^ The position of the newly placed number
      -> Value      -- ^ What value to place
      -> Board      -- ^ The current board
      -> Board      -- ^ The board after placing the number
place (r, c) val = Board . place' . unBoard where
    place' vec = vec V.// [(r, vec V.! r V.// [(c, val)])]

-- | Computer places a 2 or 4 on a free space.
playComputer :: GameState -> IO GameState
playComputer (board, score) = do
    board' <- placeRandom board
    return (board', score)

placeRandom :: Board -> IO Board
placeRandom board = do
    pos <- runMaybeT $ choose $ freePositions board
    (Just val) <- runMaybeT $ choose $ 2 : replicate 9 1
    return $ fromMaybe board $ fmap (\p -> place p val board) pos

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

-- | The game is over when there is no possible move.
gameOver :: Board -> Bool
gameOver = null . possibleMoves

-- | Can the computer put a new number at the given position?
free :: Position -> Board -> Bool
free (r, c) = (== 0) . (\vec -> (vec V.! r) V.! c) . unBoard

-- | Which positions are still free?
freePositions :: Board -> [Position]
freePositions board = filter (flip free board) positions where
    positions = [ (i,j) | i <- [0 .. r-1], j <- [0 .. c-1] ]
    (r, c) = dimensions board

moveR :: GameState -> GameState
moveR (board, score) = (Board vec', score + V.sum scores) where
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

-- | Get a board's dimensions.
--
--   >>> dimensions (emptyBoard (4,4))
--   (4,4)
dimensions :: Board -> Dimensions
dimensions board
    | V.null vec = (0, 0)
    | otherwise  = (V.length vec, V.length (vec V.! 0))
    where
        vec = unBoard board

-- | Generate an empty board with the given dimensions.
emptyBoard :: Dimensions -> Board
emptyBoard (r, c) = Board $ V.replicate r $ V.replicate c 0

instance Show Board where
    show = showBoard

showBoard :: Board -> String
showBoard = V.foldl (\s -> \v -> s ++ ('\n' : (showRow v))) "" . unBoard where
    showRow = concat . V.toList . V.map showNum
    showNum 0 = replicate 5 ' ' ++ "0"
    showNum x = let s = show ((2::Value)^x) in (replicate (6 - length s) ' ') ++ s
