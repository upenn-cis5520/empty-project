module ChessStepper where

import ChessGame
import ChessParser
import ChessSyntax
import Control.Monad.State qualified as S
import Data.List as List

-------------
-- Stepper --
-------------

data Stepper = Stepper
  { game :: Game,
    history :: Maybe Stepper
  }

initialStepper :: Stepper
initialStepper =
  Stepper
    { game = initialGame,
      history = Nothing
    }

-- take moves, and print the current setup at each turn
stepper :: IO ()
stepper = do
  putStrLn "Welcome to Chess Parser! Enter :u to undo, :q to quit, :f to load a file, and :r to restart"
  go initialStepper

go :: Stepper -> IO ()
go s = do
  putStrLn "Enter a move:"
  input <- getLine
  case List.uncons (words input) of
    Just (":f", [fn]) -> do
      f <- readFile fn
      m <- parseFile f
      movesStepper s m
    Just (":q", _) -> do
      putStrLn "Goodbye!"
    Just (":u", _) -> do
      putStrLn "Undoing..."
      case history s of
        Nothing -> do
          putStrLn "No history to undo"
          go s
        Just h -> do
          putStrLn (printGame (game h))
          go h
    Just (":r", _) -> do
      putStrLn "Restarting..."
      go initialStepper
    Just _ -> do
      movesStepper s (parseMoves input)
    Nothing -> do
      putStrLn "Please enter an input"
      go s

movesStepper :: Stepper -> Either a [Move] -> IO ()
movesStepper s (Left err) = do
  putStrLn "Invalid move format"
  go s
movesStepper s (Right m) = do
  let (result, newGame) = S.runState (playMoves m) (game s)
   in case result of
        InvalidMove -> do
          putStrLn "Invalid move"
          go s
        Draw -> do
          putStrLn "Draw"
        Won c -> do
          putStrLn (show c ++ " won!")
        ContinueGame -> do
          let newStepper = Stepper {game = newGame, history = Just s}
          putStrLn (printGame newGame)
          go newStepper