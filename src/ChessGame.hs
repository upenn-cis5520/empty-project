module ChessGame (playMove, playMoves, printGame, initialGame) where

import ChessParser
import ChessSyntax
import Control.Monad.State qualified as S
import Data.Char (chr, ord)
import Data.Foldable (find)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)

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
  checkPiece (translateColor c Pawn) (rank + translateMove c) (succ file) b
    || checkPiece (translateColor c Pawn) (rank + translateMove c) (pred file) b

-- check if a knight of a color can attack at this location
knightKills :: Board -> Color -> Square -> Bool
knightKills b c (Square rank file) =
  checkPiece (translateColor c Knight) (succ2 rank) (succ file) b
    || checkPiece (translateColor c Knight) (succ2 rank) (pred file) b
    || checkPiece (translateColor c Knight) (pred2 rank) (succ file) b
    || checkPiece (translateColor c Knight) (pred2 rank) (pred file) b
    || checkPiece (translateColor c Knight) (succ rank) (pred2 file) b
    || checkPiece (translateColor c Knight) (pred rank) (pred2 file) b
    || checkPiece (translateColor c Knight) (succ rank) (succ2 file) b
    || checkPiece (translateColor c Knight) (pred rank) (succ2 file) b

-- check if a bishop of a color can attack at this location
bishopKills :: Board -> Color -> Square -> Bool
bishopKills b c (Square rank file) =
  checkPiece (translateColor c Bishop) (succ rank) (succ file) b
    || checkPiece (translateColor c Bishop) (succ rank) (pred file) b
    || checkPiece (translateColor c Bishop) (pred rank) (succ file) b
    || checkPiece (translateColor c Bishop) (pred rank) (pred file) b
    || checkPieceEmptyPath b (translateColor c Bishop) (Square (succ rank) (succ file)) succ succ
    || checkPieceEmptyPath b (translateColor c Bishop) (Square (succ rank) (pred file)) succ pred
    || checkPieceEmptyPath b (translateColor c Bishop) (Square (pred rank) (succ file)) pred succ
    || checkPieceEmptyPath b (translateColor c Bishop) (Square (pred rank) (pred file)) pred pred

-- check if a rook of a color can attack at this location
rookKills :: Board -> Color -> Square -> Bool
rookKills b c (Square rank file) =
  checkPiece (translateColor c Rook) (succ rank) file b
    || checkPiece (translateColor c Rook) (pred rank) file b
    || checkPiece (translateColor c Rook) rank (succ file) b
    || checkPiece (translateColor c Rook) rank (pred file) b
    || checkPieceEmptyPath b (translateColor c Rook) (Square (succ rank) file) succ id
    || checkPieceEmptyPath b (translateColor c Rook) (Square (pred rank) file) pred id
    || checkPieceEmptyPath b (translateColor c Rook) (Square rank (succ file)) id succ
    || checkPieceEmptyPath b (translateColor c Rook) (Square rank (pred file)) id pred

-- check if a king of a color can attack at this location
kingKills :: Board -> Color -> Square -> Bool
kingKills b c (Square rank file) =
  checkPiece (translateColor c King) (succ rank) file b
    || checkPiece (translateColor c King) (pred rank) file b
    || checkPiece (translateColor c King) rank (succ file) b
    || checkPiece (translateColor c King) rank (pred file) b
    || checkPiece (translateColor c King) (succ rank) (succ file) b
    || checkPiece (translateColor c King) (succ rank) (pred file) b
    || checkPiece (translateColor c King) (pred rank) (succ file) b
    || checkPiece (translateColor c King) (pred rank) (pred file) b

-- check if a queen of a color can attack at this location
queenKills :: Board -> Color -> Square -> Bool
queenKills b c (Square rank file) =
  checkPiece (translateColor c Queen) (succ rank) file b
    || checkPiece (translateColor c Queen) (pred rank) file b
    || checkPiece (translateColor c Queen) rank (succ file) b
    || checkPiece (translateColor c Queen) rank (pred file) b
    || checkPiece (translateColor c Queen) (succ rank) (succ file) b
    || checkPiece (translateColor c Queen) (succ rank) (pred file) b
    || checkPiece (translateColor c Queen) (pred rank) (succ file) b
    || checkPiece (translateColor c Queen) (pred rank) (pred file) b
    || checkPieceEmptyPath b (translateColor c Queen) (Square (succ rank) file) succ id
    || checkPieceEmptyPath b (translateColor c Queen) (Square (pred rank) file) pred id
    || checkPieceEmptyPath b (translateColor c Queen) (Square rank (succ file)) id succ
    || checkPieceEmptyPath b (translateColor c Queen) (Square rank (pred file)) id pred
    || checkPieceEmptyPath b (translateColor c Queen) (Square (succ rank) (succ file)) succ succ
    || checkPieceEmptyPath b (translateColor c Queen) (Square (succ rank) (pred file)) succ pred
    || checkPieceEmptyPath b (translateColor c Queen) (Square (pred rank) (succ file)) pred succ
    || checkPieceEmptyPath b (translateColor c Queen) (Square (pred rank) (pred file)) pred pred

-- checks if a piece exists at the square and nothing goes between it and the destination
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
translateColor c = CPiece (otherColor c)

-- return opposite color
otherColor :: Color -> Color
otherColor White = Black
otherColor Black = White

-- translate the move for a color
translateMove :: Color -> Int
translateMove White = 1
translateMove Black = -1

-- Given a move, check if it results in checkmate
isCheckmate :: Game -> Bool
isCheckmate (Game b c) = case findPiece (CPiece c King) b of
  Nothing -> True
  Just s@(Square rank file) ->
    isCheck (Game (helper id id) c)
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
        if rankOp rank < 1 || rankOp rank > 8 || fileOp file < 'a' || fileOp file > 'h'
          then Map.empty
          else
            Map.insert
              (Square (rankOp rank) (fileOp file))
              (CPiece c King)
              (Map.delete s b)

-- Process an arbitrary move
boardAfterMove :: CPiece -> Square -> Square -> Board -> Board
boardAfterMove p origin destination b = Map.insert destination p (Map.delete origin b)

-- Given a game, check if the current player can move a king without a check
canMoveKing :: Game -> Square -> Bool
canMoveKing (Game b c) dest =
  let king = findPiece (CPiece c King) b
   in case king of
        Nothing -> False
        Just (Square r f) -> not (isCheck (Game (boardAfterMove (CPiece c King) (Square r f) dest b) c))

-- Given a game and destination/intermediary squares, check if the current player can castle
castlingHelper :: Game -> Square -> Square -> Square -> Square -> Maybe Square -> Bool
castlingHelper
  g@(Game b c)
  k@(Square rk fk)
  r@(Square rr fr)
  ki@(Square rki fki)
  kd@(Square rkd fkd)
  kii =
    checkPiece (CPiece c King) rk fk b
      && checkPiece (CPiece c Rook) rr fr b
      && isNothing (b !? ki)
      && isNothing (b !? kd)
      && not (isCheck g)
      && canMoveKing g ki
      && canMoveKing g kd
      && case kii of
        Nothing -> True
        Just interS -> isNothing (b !? interS)

-- Given a board and move, check if the square is either empty or has an enemey piece
emptyOrOpponent :: Game -> Square -> Bool
emptyOrOpponent (Game b c) s = case b !? s of
  Nothing -> True
  Just (CPiece c' _) -> c /= c'

-- Given a move, check if it is valid
validMove :: Move -> Game -> Bool
validMove KingSideCastling g@(Game b c) = case c of
  White -> castlingHelper g (Square 1 'e') (Square 1 'h') (Square 1 'f') (Square 1 'g') Nothing
  Black -> castlingHelper g (Square 8 'e') (Square 8 'h') (Square 8 'f') (Square 8 'g') Nothing
validMove QueenSideCastling g@(Game b c) = case c of
  White -> castlingHelper g (Square 1 'e') (Square 1 'a') (Square 1 'd') (Square 1 'c') (Just (Square 1 'b'))
  Black -> castlingHelper g (Square 8 'e') (Square 8 'a') (Square 8 'd') (Square 8 'c') (Just (Square 8 'b'))
validMove
  ( NormalMove
      p
      dest@(Square rd fd)
      disam
      (Promotion prom)
      (Capture capture)
      (Check check)
      (Mate checkm)
    )
  g@(Game b c) =
    let origin@(Square ro fo) = resolveDisambiguation disam g
        newPiece = fromMaybe p prom
        newBoard = boardAfterMove (CPiece c newPiece) origin dest b
        newGame = Game newBoard (otherColor c)
     in checkPiece (CPiece c p) rd fd b
          && emptyOrOpponent g dest
          && check == isCheck newGame
          && not (isCheck (Game newBoard c)) -- your king shouldn't be in check after the move
          && checkm == isCheckmate newGame
          && capture == isJust (b !? dest)
          && isJust prom
            == ( (p == Pawn)
                   && rd == (case c of White -> 8; Black -> 1)
               )
          && case p of
            Pawn -> validPawnMove g origin dest capture
            Knight -> validKnightMove origin dest
            Bishop -> validBishopMove b origin dest
            Rook -> validRookMove b origin dest
            Queen -> validBishopMove b origin dest || validRookMove b origin dest
            King -> validKingMove origin dest

-- For a piece check if it can make this move
validPawnMove :: Game -> Square -> Square -> Bool -> Bool
validPawnMove (Game b c) (Square ro fo) (Square rd fd) capture =
  if capture
    then rd == ro + translateMove c && (fd == succ fo || fd == pred fo)
    else
      fd == fo
        && ( rd == ro + translateMove c
               || ( ro == (case c of White -> 2; Black -> 7)
                      && rd == ro + 2 * translateMove c
                      && isNothing (b !? Square (ro + translateMove c) fo) -- the middle square is empty
                  )
           )

-- For a knight check if it can make this move
validKnightMove :: Square -> Square -> Bool
validKnightMove (Square ro fo) (Square rd fd) =
  ((rd == succ2 ro || rd == pred2 ro) && (fd == succ fo || fd == pred fo))
    || ((rd == succ ro || rd == pred ro) && (fd == succ2 fo || fd == pred2 fo))

-- For a bishop check if it can make this move
validBishopMove :: Board -> Square -> Square -> Bool
validBishopMove b (Square ro fo) (Square rd fd) =
  singlePath b (Square ro fo) (Square rd fd) succ succ
    || singlePath b (Square ro fo) (Square rd fd) succ pred
    || singlePath b (Square ro fo) (Square rd fd) pred succ
    || singlePath b (Square ro fo) (Square rd fd) pred pred

-- For a rook check if it can make this move
validRookMove :: Board -> Square -> Square -> Bool
validRookMove b (Square ro fo) (Square rd fd) =
  singlePath b (Square ro fo) (Square rd fd) succ id
    || singlePath b (Square ro fo) (Square rd fd) pred id
    || singlePath b (Square ro fo) (Square rd fd) id succ
    || singlePath b (Square ro fo) (Square rd fd) id pred

-- For a king check if it can make this move
validKingMove :: Square -> Square -> Bool
validKingMove (Square ro fo) (Square rd fd) =
  (rd == succ ro || rd == pred ro || rd == ro)
    && (fd == succ fo || fd == pred fo || fd == fo)

-- checks if a direct and empty path exists
singlePath :: Board -> Square -> Square -> (Int -> Int) -> (Char -> Char) -> Bool
singlePath b so@(Square ro fo) sd@(Square rd fd) rankOp fileOp =
  let new_ro = rankOp ro
      new_fo = fileOp fo
   in (new_ro == rd && new_fo == fd)
        || ( not (new_ro < 1 || new_ro > 8 || new_fo < 'a' || new_fo > 'h')
               && isNothing (b !? Square new_ro new_fo) -- empty square
               && singlePath b (Square new_ro new_fo) (Square rd fd) rankOp fileOp
           )

pred2 :: (Enum a) => a -> a
pred2 = pred . pred

succ2 :: (Enum a) => a -> a
succ2 = succ . succ

-- TODO: Resolve disambiguation
resolveDisambiguation :: Maybe Disambiguation -> Game -> Square
resolveDisambiguation (Just (Both s)) _ = s undefined

-- Given a game, switch the current player
switchPlayer :: Game -> Game
switchPlayer (Game b White) = Game b Black
switchPlayer (Game b Black) = Game b White

-- Given a move, update the new game state
playMove :: Move -> S.State Game MoveResult
playMove m = do
  g@(Game b c) <- S.get
  if validMove m g
    then do
      let Game b' c' = Game (updateBoard m g) (otherColor c)
      S.put (Game b' c')
      if isCheckmate (Game b' c')
        then return (Won c)
        else return ContinueGame
    else return InvalidMove

-- Given a move and a board, update the board
updateBoard :: Move -> Game -> Board
updateBoard KingSideCastling (Game b c) =
  let r = case c of White -> 1; Black -> 8
   in Map.insert
        (Square r 'g')
        (CPiece c King)
        ( Map.insert
            (Square r 'f')
            (CPiece c Rook)
            (Map.delete (Square r 'e') (Map.delete (Square r 'h') b))
        )
updateBoard QueenSideCastling (Game b c) =
  let r = case c of White -> 1; Black -> 8
   in Map.insert
        (Square r 'c')
        (CPiece c King)
        ( Map.insert
            (Square r 'd')
            (CPiece c Rook)
            (Map.delete (Square r 'e') (Map.delete (Square r 'a') b))
        )
updateBoard (NormalMove p dest disam _ _ _ _) g@(Game b c) =
  let origin = resolveDisambiguation disam g
   in boardAfterMove (CPiece c p) origin dest b

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
    else val ++ " " ++ printRow' i (succ c) b
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
