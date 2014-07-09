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
move U = first Field . move' . first unField
move R = first (Field . transpose) . move' . first (transpose . unField)
move D = first (Field . mirrorH) . move' . first (mirrorH . unField)
move L = first (Field . transpose . mirrorH) . move' . first (mirrorH . transpose. unField)

move' (field, score) = (field', score + V.sum scores) where
    state' = V.map (combine . compact) field
    (field', scores) = V.unzip state'

transpose :: V.Vector (V.Vector Value) -> V.Vector (V.Vector Value)
transpose field
    | V.null field = field
    | otherwise    = V.generate h (\i -> V.generate w (\j -> (field V.! j) V.! i))
    where
        w = V.length field
        h = V.length (field V.! 0)

mirrorH :: V.Vector (V.Vector Value) -> V.Vector (V.Vector Value)
mirrorH field = V.generate w (\i -> field V.! (w - i - 1)) where
    w = V.length field

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
