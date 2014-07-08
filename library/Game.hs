-- library/Game.hs
-- | The game logic for 2048.
module Game (
      Field
    , State
    , move
    , transpose
    , compact
    , combine
    ) where

import Control.Arrow (first)

import qualified Data.Vector as V

-- | Models the 2048 game field as a vector of int vectors.
type Field = V.Vector (V.Vector Value)
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

move' (field, score) = (field', score + V.sum scores) where
    state' = V.map (combine . compact) field
    (field', scores) = V.unzip state'

transpose :: Field -> Field
transpose field
    | V.null field = field
    | otherwise    = V.generate h (\i -> V.generate w (\j -> (field V.! j) V.! i))
    where
        w = V.length field
        h = V.length (field V.! 0)

mirrorH :: Field -> Field
mirrorH field = V.generate w (\i -> field V.! (w - i - 1)) where
    w = V.length field

compact :: V.Vector Value -> V.Vector Value
compact = undefined

combine :: V.Vector Value -> (V.Vector Value, Int)
combine = undefined

initial :: Int      -- ^ Width of the game field
        -> Int      -- ^ Height of the game field
        -> Field    -- ^ Generated game field
initial x y = V.generate x (const V.replicate y 0)

example :: Field
example = V.fromList (map V.fromList m) where
    m = map (\n -> [n*3 .. (n*3+4)]) [1 .. 3]
