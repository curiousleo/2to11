-- library/AI.hs
-- | The AI player for 2048.
module AI (
      baselineAI
    , runEval
    , eval
    ) where

import Prelude hiding (mapM)

import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)

import Data.List (find)
import Data.Maybe (fromMaybe)

import Game

type AI = Board -> Direction

-- | The evaluation monad.
type Eval = ReaderT AI (StateT GameState IO)

-- | Evaluate an AI on a board of given dimensions.
--
--   Example:
--
--   >>> runEval (4, 4) baselineAI eval
--   3067
runEval :: Dimensions -> AI -> Eval a -> IO a
runEval dims ai ev = do
    startState <- playComputer <=< playComputer $ (emptyBoard dims, 0)
    runEval' startState ai ev

-- | Evaluate an AI given a starting state.
runEval' :: GameState -> AI -> Eval a -> IO a
runEval' st ai ev = evalStateT (runReaderT ev ai) st

-- | Play a game, return final score
eval :: Eval Score
eval = do
    ai <- ask
    state@(board, score) <- lift get
    if null $ possibleMoves board
        then return score
        else do
            state' <- lift . lift . playComputer $ playAI ai state
            lift . put $ state'
            eval

-- | Very simple AI player: chooses the first of Up, Right, Down that is
--   possible (using `canMove`); otherwise plays Left.
baselineAI :: AI
baselineAI board = fromMaybe L $ find (flip canMove board) [U, R, D]

-- | Let the AI make a move.
playAI :: AI -> GameState -> GameState
playAI ai st@(board, _) = move (ai board) st
