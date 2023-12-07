module ChessGame (playMove, playMoves, printGame, initialGame) where

import ChessParser
import ChessSyntax
import Control.Monad.State qualified as S
import Data.Char (chr, ord)
import Data.Foldable (find)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)

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
  Just (Square rank file) ->
    pawnKills b c (Square rank file)
      || knightKills b c (Square rank file)
      || bishopKills b c (Square rank file)
      || rookKills b c (Square rank file)
      || kingKills b c (Square rank file)
      || queenKills b c (Square rank file)

-- check if a pawn of a color can attack at this location
pawnKills :: Board -> Color -> Square -> Bool
pawnKills b c (Square rank file) =
  checkPiece (translateColor c Pawn) (rank + translateMove c) (chr (ord file + 1)) b
    || checkPiece (translateColor c Pawn) (rank + translateMove c) (chr (ord file - 1)) b

-- check if a knight of a color can attack at this location
knightKills :: Board -> Color -> Square -> Bool
knightKills b c (Square rank file) =
  checkPiece (translateColor c Knight) (rank + 2) (chr (ord file + 1)) b
    || checkPiece (translateColor c Knight) (rank + 2) (chr (ord file - 1)) b
    || checkPiece (translateColor c Knight) (rank - 2) (chr (ord file + 1)) b
    || checkPiece (translateColor c Knight) (rank - 2) (chr (ord file - 1)) b
    || checkPiece (translateColor c Knight) (rank + 1) (chr (ord file + 2)) b
    || checkPiece (translateColor c Knight) (rank + 1) (chr (ord file - 2)) b
    || checkPiece (translateColor c Knight) (rank - 1) (chr (ord file + 2)) b
    || checkPiece (translateColor c Knight) (rank - 1) (chr (ord file - 2)) b

-- TODO: Check if moving any of these leads to checkmate
-- check if a bishop of a color can attack at this location
bishopKills :: Board -> Color -> Square -> Bool
bishopKills b c (Square rank file) =
  checkPiece (translateColor c Bishop) (rank + 1) (chr (ord file + 1)) b
    || checkPiece (translateColor c Bishop) (rank + 1) (chr (ord file - 1)) b
    || checkPiece (translateColor c Bishop) (rank - 1) (chr (ord file + 1)) b
    || checkPiece (translateColor c Bishop) (rank - 1) (chr (ord file - 1)) b
    || checkPieceEmptyPath b (translateColor c Bishop) (Square (rank + 1) (chr (ord file + 1))) succ succ
    || checkPieceEmptyPath b (translateColor c Bishop) (Square (rank + 1) (chr (ord file - 1))) succ pred
    || checkPieceEmptyPath b (translateColor c Bishop) (Square (rank - 1) (chr (ord file + 1))) pred succ
    || checkPieceEmptyPath b (translateColor c Bishop) (Square (rank - 1) (chr (ord file - 1))) pred pred

-- check if a rook of a color can attack at this location
rookKills :: Board -> Color -> Square -> Bool
rookKills b c (Square rank file) =
  checkPiece (translateColor c Rook) (rank + 1) file b
    || checkPiece (translateColor c Rook) (rank - 1) file b
    || checkPiece (translateColor c Rook) rank (chr (ord file + 1)) b
    || checkPiece (translateColor c Rook) rank (chr (ord file - 1)) b
    || checkPieceEmptyPath b (translateColor c Rook) (Square (rank + 1) file) succ id
    || checkPieceEmptyPath b (translateColor c Rook) (Square (rank - 1) file) pred id
    || checkPieceEmptyPath b (translateColor c Rook) (Square rank (chr (ord file + 1))) id succ
    || checkPieceEmptyPath b (translateColor c Rook) (Square rank (chr (ord file - 1))) id pred

-- check if a king of a color can attack at this location
kingKills :: Board -> Color -> Square -> Bool
kingKills b c (Square rank file) =
  checkPiece (translateColor c King) (rank + 1) file b
    || checkPiece (translateColor c King) (rank - 1) file b
    || checkPiece (translateColor c King) rank (chr (ord file + 1)) b
    || checkPiece (translateColor c King) rank (chr (ord file - 1)) b
    || checkPiece (translateColor c King) (rank + 1) (chr (ord file + 1)) b
    || checkPiece (translateColor c King) (rank + 1) (chr (ord file - 1)) b
    || checkPiece (translateColor c King) (rank - 1) (chr (ord file + 1)) b
    || checkPiece (translateColor c King) (rank - 1) (chr (ord file - 1)) b

-- check if a queen of a color can attack at this location
queenKills :: Board -> Color -> Square -> Bool
queenKills b c (Square rank file) =
  checkPiece (translateColor c Queen) (rank + 1) file b
    || checkPiece (translateColor c Queen) (rank - 1) file b
    || checkPiece (translateColor c Queen) rank (chr (ord file + 1)) b
    || checkPiece (translateColor c Queen) rank (chr (ord file - 1)) b
    || checkPiece (translateColor c Queen) (rank + 1) (chr (ord file + 1)) b
    || checkPiece (translateColor c Queen) (rank + 1) (chr (ord file - 1)) b
    || checkPiece (translateColor c Queen) (rank - 1) (chr (ord file + 1)) b
    || checkPiece (translateColor c Queen) (rank - 1) (chr (ord file - 1)) b
    || checkPieceEmptyPath b (translateColor c Queen) (Square (rank + 1) file) succ id
    || checkPieceEmptyPath b (translateColor c Queen) (Square (rank - 1) file) pred id
    || checkPieceEmptyPath b (translateColor c Queen) (Square rank (chr (ord file + 1))) id succ
    || checkPieceEmptyPath b (translateColor c Queen) (Square rank (chr (ord file - 1))) id pred
    || checkPieceEmptyPath b (translateColor c Queen) (Square (rank + 1) (chr (ord file + 1))) succ succ
    || checkPieceEmptyPath b (translateColor c Queen) (Square (rank + 1) (chr (ord file - 1))) succ pred
    || checkPieceEmptyPath b (translateColor c Queen) (Square (rank - 1) (chr (ord file + 1))) pred succ
    || checkPieceEmptyPath b (translateColor c Queen) (Square (rank - 1) (chr (ord file - 1))) pred pred

-- checks if a piece exists at the square and nothing does between it and the destination
checkPieceEmptyPath :: Board -> CPiece -> Square -> (Int -> Int) -> (Char -> Char) -> Bool
checkPieceEmptyPath b (CPiece c p) (Square rankO fileO) rankOp fileOp =
  let rankT = rankOp rankO
      fileT = fileOp fileO
   in ( not (rankT < 1 || rankT > 8 || fileT < 'a' || fileT > 'h')
          && ( ( checkPiece (CPiece c p) rankT fileT b
                   && isNothing (b !? Square rankO fileO)
               )
                 || checkPieceEmptyPath b (CPiece c p) (Square rankT fileT) rankOp fileOp
             )
      )

-- checks if a piece exists at the square
checkPiece :: CPiece -> Rank -> File -> Board -> Bool
checkPiece p r f b = case b !? Square r f of
  Nothing -> False
  Just p' -> p' == p

-- Changes the color
translateColor :: Color -> Piece -> CPiece
translateColor White p = CPiece Black p
translateColor Black p = CPiece White p

-- translate the move for a color
translateMove :: Color -> Int
translateMove White = 1
translateMove Black = -1

-- Given a move, check if it results in checkmate
isCheckmate :: Game -> Bool
isCheckmate (Game b c) = case findPiece (CPiece c King) b of
  Nothing -> True
  Just s@(Square rank file) ->
    isCheck (Game b c)
      && isCheck (Game (helper succ id) c)
      && isCheck (Game (helper pred id) c)
      && isCheck (Game (helper succ succ) c)
      && isCheck (Game (helper succ pred) c)
      && isCheck (Game (helper pred pred) c)
      && isCheck (Game (helper pred succ) c)
      && isCheck (Game (helper id succ) c)
      && isCheck (Game (helper id pred) c)
    where
      helper :: (Int -> Int) -> (Char -> Char) -> Board
      helper rankOp fileOp =
        Map.insert
          (Square (rankOp rank) (fileOp file))
          (CPiece c King)
          (Map.delete s b)

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
    val = maybe "." show (Map.lookup (Square i c) b)

-- Initialise the game
firstRow :: String
firstRow = "   a b c d e f g h   "

secondRow :: String
secondRow = "  +---------------+  "

initialList :: [(Square, CPiece)]
initialList =
  [ (Square 1 'a', CPiece White Rook),
    (Square 1 'b', CPiece White Knight),
    (Square 1 'c', CPiece White Bishop),
    (Square 1 'd', CPiece White Queen),
    (Square 1 'e', CPiece White King),
    (Square 1 'f', CPiece White Bishop),
    (Square 1 'g', CPiece White Knight),
    (Square 1 'h', CPiece White Rook),
    (Square 2 'a', CPiece White Pawn),
    (Square 2 'b', CPiece White Pawn),
    (Square 2 'c', CPiece White Pawn),
    (Square 2 'd', CPiece White Pawn),
    (Square 2 'e', CPiece White Pawn),
    (Square 2 'f', CPiece White Pawn),
    (Square 2 'g', CPiece White Pawn),
    (Square 2 'h', CPiece White Pawn),
    (Square 7 'a', CPiece Black Pawn),
    (Square 7 'b', CPiece Black Pawn),
    (Square 7 'c', CPiece Black Pawn),
    (Square 7 'd', CPiece Black Pawn),
    (Square 7 'e', CPiece Black Pawn),
    (Square 7 'f', CPiece Black Pawn),
    (Square 7 'g', CPiece Black Pawn),
    (Square 7 'h', CPiece Black Pawn),
    (Square 8 'a', CPiece Black Rook),
    (Square 8 'b', CPiece Black Knight),
    (Square 8 'c', CPiece Black Bishop),
    (Square 8 'd', CPiece Black Queen),
    (Square 8 'e', CPiece Black King),
    (Square 8 'f', CPiece Black Bishop),
    (Square 8 'g', CPiece Black Knight),
    (Square 8 'h', CPiece Black Rook)
  ]

initialGame :: Game
initialGame = Game (Map.fromList initialList) White
