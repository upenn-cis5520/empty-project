module ChessStepper (playMove, playMoves) where

import ChessParser
import ChessSyntax
import qualified Control.Monad.State as S

-- Given a player, check if they are in check
isCheck :: Move -> S.State Game Bool
isCheck = undefined

-- Given a move, check if it results in checkmate
isCheckmate :: Move -> S.State Game Bool
isCheckmate = undefined

-- Given a move, check if it is valid
validMove :: Move -> S.State Game Bool
validMove = undefined

-- Given a game, switch the current player
switchPlayer :: S.State Game ()
switchPlayer = undefined

-- Given a move, update the new game state
playMove :: Move -> S.State Game ()
playMove = undefined

-- Given a list of moves, play them all
playMoves :: [Move] -> S.State Game ()
playMoves = undefined

-- Initialise the game
createGame :: Game
createGame = undefined

-- Print the current Game
printGame :: S.State Game ()
printGame = undefined

data Stepper = Stepper
  { game :: Game,
    move :: Maybe Move,
    history :: Maybe Stepper
  }

initialStepper :: Stepper
initialStepper =
  Stepper
    { game = createGame,
      move = Nothing,
      history = Nothing
    }

-- take moves, and print the current setup at each turn
stepper :: IO ()
stepper = undefined
