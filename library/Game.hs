-- library/Game.hs
-- | The game logic for 2048.
module Game (
      Direction (..)
    , Field (..)
    , Position
    , State
    , Value

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
import Control.Monad (mzero)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Class (lift)

import qualified Data.Vector as V
import System.Random (getStdRandom, randomR)

-- | Models the 2048 game field as a vector of int vectors.
newtype Field = Field { unField :: V.Vector (V.Vector Value) }
    deriving Eq
type Value = Int

-- | A position on the game field.
type Position = (Int, Int)

-- | The state, modelled as the game field and the current score.
type State = (Field, Int)

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
      -> Field      -- ^ The current state
      -> Field      -- ^ The next state
place (r, c) val = Field . place' . unField where
    place' vec = vec V.// [(r, vec V.! r V.// [(c, val)])]

playComputer :: State -> MaybeT IO State
playComputer (field, score) = do
    field' <- placeRandom field
    return (field', score)

placeRandom :: Field -> MaybeT IO Field
placeRandom field = do
    pos <- choose $ freePositions field
    val <- choose [1, 2]
    return $ place pos val field

choose :: [a] -> MaybeT IO a
choose [] = mzero
choose xs = do
    let range = (0, length xs - 1)
    i <- lift $ getStdRandom $ randomR range
    return $ xs !! i

-- | Can the player move in the given direction?
canMove :: Direction -> Field -> Bool
canMove dir field = 0 /= score where
    score = snd $ move dir (field, 0)

-- | Which moves are possible?
possibleMoves :: Field -> [Direction]
possibleMoves field = filter (flip canMove field) [R, U, L, D]

-- | Can the computer put a new number at the given position?
free :: Position -> Field -> Bool
free (r, c) = (== 0) . (\vec -> (vec V.! r) V.! c) . unField

-- | Which positions are still free?
freePositions :: Field -> [Position]
freePositions field = filter (flip free field) positions where
    positions = [ (i,j) | i <- [0 .. r-1], j <- [0 .. c-1] ]
    (r, c) = dimensions field

move' :: State -> State
move' (field, score) = (Field vec', score + V.sum scores) where
    (_, w) = dimensions field
    combined = V.map (first (restore w) . compact) (unField field)
    (vec', scores) = V.unzip combined

rot90, rot180, rot270 :: Field -> Field
rot90 field = Field rotated where
    rotated = V.generate w (\i -> V.generate h (\j -> rot90' i j))
    rot90' i j = ((unField field) V.! j) V.! (w-i-1)
    (h, w) = dimensions field
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

dimensions :: Field -> (Int, Int)
dimensions field
    | V.null vec = (0, 0)
    | otherwise  = (V.length vec, V.length (vec V.! 0))
    where
        vec = unField field

initial :: Int      -- ^ Width of the game field
        -> Int      -- ^ Height of the game field
        -> Field    -- ^ Generated game field
initial x y = Field $ V.generate x (const V.replicate y 0)

example :: Field
example = Field $ V.fromList (map V.fromList m) where
    m = [[0, 1, 0, 1, 2],
         [0, 1, 1, 2, 3],
         [1, 1, 2, 2, 2]]

instance Show Field where
    show = showField

showField :: Field -> String
showField = V.foldl (\s -> \v -> s ++ ('\n' : (showRow v))) "" . unField where
    showRow = concat . V.toList . V.map showNum
    showNum 0 = replicate 5 ' ' ++ "0"
    showNum x = let s = show ((2::Value)^x) in (replicate (6 - length s) ' ') ++ s
