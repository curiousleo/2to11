-- library/AI.hs
-- | The AI player for 2048.
module AI (
      AI
    , Eval

    , play

    , runEval
    , runEval'
    , eval
    , evalRound

    , baselineAI
    ) where

import Prelude hiding (mapM)

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (StateT, runStateT, get, put)

import Data.List (find)
import Data.Maybe (fromMaybe)

import Game

-- | The type of an AI policy, mapping the state (of the board) to an action (a
--   move in a certain direction).
type AI = Board -> Direction

-- | The evaluation monad.
type Eval = ReaderT AI (StateT GameState IO)

-- | Evaluate an AI given an initial state.
runEval :: Eval a -> GameState -> AI -> IO (a, GameState)
runEval ev st ai = runStateT (runReaderT ev ai) st

-- | Run the Eval monad.
--
--   >>> runEval' eval (4, 4) baselineAI
--   ((),(
--        2    32     8   256
--        4    64    32    64
--       16     8    16    32
--        2     4     8    16,2932))
runEval' :: Eval a               -- ^ Eval monad
        -> Dimensions           -- ^ Board dimensions
        -> AI                   -- ^ Move policy
        -> IO (a, GameState)    -- ^ Resulting value and game state
runEval' ev dims ai = do
    state <- initialGameState dims
    runEval ev state ai

-- | Play a game of 2048 to the end.
eval :: Eval ()
eval = do
    ai <- ask
    state@(board, _) <- lift get
    if gameOver board
        then return ()
        else do
            state' <- lift . lift . playComputer $ playAI ai state
            lift . put $ state'
            eval

-- | Play one round of 2048.
evalRound :: Eval ()
evalRound = do
    ai <- ask
    state <- lift get
    state' <- lift . lift . playComputer $ playAI ai state
    lift . put $ state'
    return ()

-- | Play a game of 2048 and return the final score.
play :: Dimensions  -- ^ Board dimensions
     -> AI          -- ^ Move policy
     -> IO Score    -- ^ Final score
play = liftM (liftM (snd . snd)) . runEval' eval

-- | Very simple AI player: chooses the first of Up, Right, Down that is
--   possible (using `canMove`); otherwise plays Left.
baselineAI :: AI
baselineAI board = fromMaybe L $ find (flip canMove board) [U, R, D]

-- | Let the AI make a move.
playAI :: AI -> GameState -> GameState
playAI ai st@(board, _) = move (ai board) st
