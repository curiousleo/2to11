-- library/Game.hs
-- | The game logic for 2048.
module Game (
      Field (..)
    , State
    , move
    , transpose
    , compact
    , combine
    ) where

import Control.Arrow (first)

import qualified Data.Vector as V

-- | Models the 2048 game field as a vector of int vectors.
newtype Field = Field { unField :: V.Vector (V.Vector Value) }
type Value = Int

-- | A position on the game field.
type Position = (Int, Int)

-- | The state, modelled as the game field and the current score.
type State = (Field, Int)

-- | A player's move.
data Direction = U | R | D | L
    deriving (Show, Eq)

-- | Player move
move :: Direction   -- ^ The direction of the move
     -> State       -- ^ The current state
     -> State       -- ^ The next state
move U = move'
move R = first transpose . move' . first transpose
move D = first mirrorH . move' . first mirrorH
move L = first (transpose . mirrorH) . move' . first (mirrorH . transpose)

move' :: State -> State
move' (field, score) = (Field vec', score + V.sum scores) where
    vec = V.map (combine . compact) (unField field)
    (vec', scores) = V.unzip vec

transpose :: Field -> Field
transpose field
    | V.null (unField field) = field
    | otherwise              = Field $ inv
    where
        inv = V.generate h (\i -> V.generate w (\j -> (vec V.! j) V.! i))
        vec = unField field
        w = V.length vec
        h = V.length (vec V.! 0)

mirrorH :: Field -> Field
mirrorH field = Field $ V.generate w (\i -> vec V.! (w - i - 1)) where
    vec = unField field
    w = V.length vec

compact :: V.Vector Value -> V.Vector Value
compact = V.fromList . compact' . V.toList where
    compact' x = nonzero ++ replicate n 0 where
        nonzero = filter (/= 0) x
        n = length x - length nonzero

combine :: V.Vector Value -> (V.Vector Value, Value)
combine = undefined

initial :: Int      -- ^ Width of the game field
        -> Int      -- ^ Height of the game field
        -> Field    -- ^ Generated game field
initial x y = Field $ V.generate x (const V.replicate y 0)

example :: Field
example = Field $ V.fromList (map V.fromList m) where
    m = map (\n -> [n*3 .. (n*3+4)]) [1 .. 3]
