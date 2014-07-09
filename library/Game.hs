-- library/Game.hs
-- | The game logic for 2048.
module Game (
      Field (..)
    , State
    , rot90
    , move
    , compact
    ) where

import Control.Arrow (first)

import qualified Data.Vector as V

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

-- | Player move
move :: Direction   -- ^ The direction of the move
     -> State       -- ^ The current state
     -> State       -- ^ The next state
move R = move'
move U = first rot90  . move' . first rot270
move L = first rot180 . move' . first rot180
move D = first rot270 . move' . first rot90

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
    show = V.foldl (\s -> \v -> s ++ ('\n' : (showRow v))) "" . unField where
        showRow = concat . V.toList . V.map showNum
        showNum 0 = replicate 5 ' ' ++ "0"
        showNum x = let s = show (2^x) in (replicate (6 - length s) ' ') ++ s
