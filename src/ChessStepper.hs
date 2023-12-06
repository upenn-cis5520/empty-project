module ChessStepper where

import ChessGame
import ChessParser
import ChessSyntax
import Data.Map qualified as Map


-------------
-- Stepper --
-------------

-- Initialise the game
initialList :: [(Square, CPiece)]
initialList =
  [ (Square 'a' 1, CPiece White Rook),
    (Square 'b' 1, CPiece White Knight),
    (Square 'c' 1, CPiece White Bishop),
    (Square 'd' 1, CPiece White Queen),
    (Square 'e' 1, CPiece White King),
    (Square 'f' 1, CPiece White Bishop),
    (Square 'g' 1, CPiece White Knight),
    (Square 'h' 1, CPiece White Rook),
    (Square 'a' 2, CPiece White Pawn),
    (Square 'b' 2, CPiece White Pawn),
    (Square 'c' 2, CPiece White Pawn),
    (Square 'd' 2, CPiece White Pawn),
    (Square 'e' 2, CPiece White Pawn),
    (Square 'f' 2, CPiece White Pawn),
    (Square 'g' 2, CPiece White Pawn),
    (Square 'h' 2, CPiece White Pawn),
    (Square 'a' 7, CPiece Black Pawn),
    (Square 'b' 7, CPiece Black Pawn),
    (Square 'c' 7, CPiece Black Pawn),
    (Square 'd' 7, CPiece Black Pawn),
    (Square 'e' 7, CPiece Black Pawn),
    (Square 'f' 7, CPiece Black Pawn),
    (Square 'g' 7, CPiece Black Pawn),
    (Square 'h' 7, CPiece Black Pawn),
    (Square 'a' 8, CPiece Black Rook),
    (Square 'b' 8, CPiece Black Knight),
    (Square 'c' 8, CPiece Black Bishop),
    (Square 'd' 8, CPiece Black Queen),
    (Square 'e' 8, CPiece Black King),
    (Square 'f' 8, CPiece Black Bishop),
    (Square 'g' 8, CPiece Black Knight),
    (Square 'h' 8, CPiece Black Rook)
  ]
createGame :: Game
createGame = Game (Map.fromList initialList) White

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
