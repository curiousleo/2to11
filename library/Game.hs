-- library/Game.hs
-- | The game logic for 2048.
module Game (
      Field
    , State
    , move
    , compact
    , combine
    ) where

import Control.Arrow (first)

import Data.Array.Repa (Z (..), (:.) (..))
import qualified Data.Array.Repa as R

-- | Models the 2048 game field as a vector of int vectors.
type Field = R.Array R.U R.DIM2 Value
type Value = Int

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
move R =
    first (R.computeS . R.transpose) . move' . first (R.computeS . R.transpose)
move D =
    first (R.computeS . mirror) . move' . first (R.computeS . mirror)
move L =
    first (R.computeS . R.transpose . mirror) . move' . first (R.computeS . mirror . R.transpose)

move' :: State -> State
move' (field, score) = (R.computeS field', score + R.sumAllS scores) where
    state' = R.traverse field id (combine . compact (R.extent field))
    (field', scores) = unzip' state'
    unzip' ar = (fsts, snds) where (fsts, snds) = (R.map fst ar, R.map snd ar)

-- mirror :: (R.Source r e) => R.Array r DIM2 e -> R.Array D DIM2 e
mirror field = R.backpermute e mir field where
    e@(Z :. x :. _) = R.extent field
    mir (Z :. i :. j) = (Z :. x - i - 1 :. j)

compact :: R.DIM2 -> (R.DIM2 -> Value) -> R.Array R.U R.DIM2 Value
compact = undefined

combine :: R.Array R.U R.DIM2 Value -> R.DIM2 -> (Value, Value)
combine = undefined

initial :: Int      -- ^ Width of the game field
        -> Int      -- ^ Height of the game field
        -> Field    -- ^ Generated game field
initial x y = R.computeS . R.fromFunction (Z :. x :. y) $ const 0

example :: Field
example = R.fromListUnboxed (Z :. (4 :: Int) :. (6 :: Int)) [1 .. 4*6]
