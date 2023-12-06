module ChessGame (playMove, playMoves, printGame) where

import ChessParser
import ChessSyntax
import Control.Monad.State qualified as S
import Data.Map qualified as Map
import Data.Char (ord, chr)

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

-- Print the current Game
printGame :: S.State Game String
printGame = do
  (Game b col) <- S.get
  return $ firstRow ++ "\n" ++ secondRow ++ "\n" ++ printRow 8 b ++ "\nIt is currently " ++ show col ++ "'s turn.\n"

printRow :: Int -> Board -> String
printRow r b = if r == 0 
  then
    secondRow ++ "\n" ++ firstRow 
  else 
    show r ++ " |" ++ printRow' r 'a' b ++ "| " 
      ++ show r ++ "\n" ++ printRow (r - 1) b

printRow' :: Int -> Char -> Board -> String
printRow' i c b =
  if c == 'h' 
    then val
  else val ++ " " ++ printRow' i (chr (ord c + 1)) b
  where
    val :: String
    val = case Map.lookup (Square c i) b of
      Just x -> show x
      Nothing -> "x"


firstRow :: String
firstRow = "   a b c d e f g h   "
secondRow :: String
secondRow = "  +---------------+  "
  