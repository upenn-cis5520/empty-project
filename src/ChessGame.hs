module ChessGame (playMove, playMoves, printGame, initialGame) where

import ChessParser
import ChessSyntax
import Control.Monad.State qualified as S
import Data.Char (chr, ord)
import Data.Foldable (find)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Maybe (isJust)

-- Find the box that a piece is in
findPiece :: CPiece -> Board -> Maybe Square
findPiece p = Map.foldrWithKey (\k v acc -> if v == p then Just k else acc) Nothing

-- Check that both kinds exist
validBoard :: Board -> Bool
validBoard b = isJust (findPiece (CPiece White King) b) && isJust (findPiece (CPiece Black King) b)

-- Given a game, check if the current player is in check
isCheck :: Game -> Bool
isCheck (Game b c) = case findPiece (CPiece c King) b of
  Nothing -> True
  Just (Square rank file) -> isCheck' k b c

-- check if a pawn of a color can attack at this location
pawnKills :: Board -> Color -> Square -> Bool
pawnKills b c (Square rank file) =
  (isJust b !? Square (chr (ord rank - 1) (file - (translateMove c))))
    || (isJust (b !? Square (chr (ord rank + 1)) (file + (translateMove c))))

-- translate the move for a color
translateMove :: Color -> Int
translateMove White = 1
translateMove Black = -1

-- Given a move, check if it results in checkmate
isCheckmate :: Game -> Bool
isCheckmate = undefined

-- Given a move, check if it is valid
validMove :: Move -> S.State Game Bool
validMove = undefined

-- Given a game, switch the current player
switchPlayer :: Game -> Game
switchPlayer (Game b White) = Game b Black
switchPlayer (Game b Black) = Game b White

-- Given a move, update the new game state
playMove :: Move -> S.State Game MoveResult
playMove = undefined

-- Given a list of moves, play them all
playMoves :: [Move] -> S.State Game MoveResult
playMoves [] = do
  g <- S.get
  return ContinueGame
playMoves (m : ms) = do
  r <- playMove m
  case r of
    ContinueGame -> playMoves ms
    _ -> return r

-- Print a Game's state
printGame :: Game -> String
printGame (Game b col) = firstRow ++ "\n" ++ secondRow ++ "\n" ++ printRow 8 b ++ "\nIt is currently " ++ show col ++ "'s turn.\n"

printRow :: Int -> Board -> String
printRow r b =
  if r == 0
    then secondRow ++ "\n" ++ firstRow
    else
      show r
        ++ " |"
        ++ printRow' r 'a' b
        ++ "| "
        ++ show r
        ++ "\n"
        ++ printRow (r - 1) b

printRow' :: Int -> Char -> Board -> String
printRow' i c b =
  if c == 'h'
    then val
    else val ++ " " ++ printRow' i (chr (ord c + 1)) b
  where
    val :: String
    val = maybe "." show (Map.lookup (Square c i) b)

-- Initialise the game
firstRow :: String
firstRow = "   a b c d e f g h   "

secondRow :: String
secondRow = "  +---------------+  "

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

initialGame :: Game
initialGame = Game (Map.fromList initialList) White
