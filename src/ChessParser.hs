module ChessParser
  ( parseMoves,
    parseSingleMove,
    parseFile,
  )
where

import ChessSyntax
import Control.Applicative (Alternative (many, (<|>)))
import Data.Maybe (fromMaybe, isJust)
import Test.HUnit (Test (TestCase, TestList), runTestTT, (~:), (~?=))
import Text.Parsec (ParseError, char, digit, eof, oneOf, option, optionMaybe, parse, spaces, string, try)
import Text.Parsec.String (Parser, parseFromFile)

-- Given pace separated moves, return a list of Moves
parseMoves :: String -> Either ParseError [Move]
parseMoves = parse movesParser ""

-- Function that actaully parse a single chess move
parseSingleMove :: String -> Either ParseError Move
parseSingleMove = parse moveParser ""

-- TODO: check that this works
parseFile :: String -> IO (Either ParseError [Move])
parseFile = parseFromFile (const <$> movesParser <*> eof)

test_SingleMove :: Test
test_SingleMove =
  TestList
    [ -- Test Pawn move
      parseMoves "e3" ~?= Right [NormalMove Pawn (Square 'e' 3) Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test Pawn move with disambiguation
      parseMoves "e7e3" ~?= Right [NormalMove Pawn (Square 'e' 3) (Just (Both (Square 'e' 7))) (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test King move
      parseMoves "Ka1b4" ~?= Right [NormalMove King (Square 'b' 4) (Just (Both (Square 'a' 1))) (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test Castling
      parseMoves "O-O" ~?= Right [KingSideCastling],
      parseMoves "O-O-O" ~?= Right [QueenSideCastling],
      -- Test Pawn Promotion
      parseMoves "ae7=Q" ~?= Right [NormalMove Pawn (Square 'e' 7) (Just (File 'a')) (Promotion (Just Queen)) (Capture False) (Check False) (Mate False)]
    ]

-- Space separated moves parser
movesParser :: Parser [Move]
movesParser = many $ do
  move <- moveParser
  spaces
  return move

-- Single move parser
moveParser :: Parser Move
moveParser =
  try queensideCastlingParser
    <|> try kingsideCastlingParser
    <|> try normalMoveParser
    <|> normalMoveWithDisambiguationParser

normalMoveParser :: Parser Move
normalMoveParser = do
  p <- optionMaybe pieceParser
  capture <- captureParser
  toSquare <- squareParser
  prom <- promotionParser
  check <- optionMaybe (char '+')
  checkmate <- optionMaybe (char '#')
  eof
  return $ NormalMove (fromMaybe Pawn p) toSquare Nothing (Promotion prom) (Capture capture) (Check (isJust check)) (Mate (isJust checkmate))

normalMoveWithDisambiguationParser :: Parser Move
normalMoveWithDisambiguationParser = do
  p <- optionMaybe pieceParser
  disambiguation <- optionMaybe (try bothParser <|> try rankParser <|> try fileParser)
  capture <- captureParser
  toSquare <- squareParser
  prom <- promotionParser
  check <- optionMaybe (char '+')
  checkmate <- optionMaybe (char '#')
  eof
  return $ NormalMove (fromMaybe Pawn p) toSquare disambiguation (Promotion prom) (Capture capture) (Check (isJust check)) (Mate (isJust checkmate))

pieceParser :: Parser Piece
pieceParser = do
  p <- oneOf "PNBRQK"
  return $ case p of
    'N' -> Knight
    'B' -> Bishop
    'R' -> Rook
    'Q' -> Queen
    'K' -> King
    _ -> error "Impossible"

squareParser :: Parser Square
squareParser = do
  file <- oneOf ['a' .. 'h'] -- File (column)
  rank <- oneOf ['1' .. '8'] -- Rank (row)
  return $ Square file (read [rank])

fileParser :: Parser Disambiguation
fileParser = do
  file <- oneOf ['a' .. 'h']
  return $ File file

rankParser :: Parser Disambiguation
rankParser = do
  rank <- digit
  return $ Rank (read [rank])

bothParser :: Parser Disambiguation
bothParser = do
  file <- oneOf ['a' .. 'h'] -- File (column)
  rank <- oneOf ['1' .. '8'] -- Rank (row)
  return $ Both $ Square file (read [rank])

captureParser :: Parser Bool
captureParser = option False (char 'x' >> return True)

promotionParser :: Parser (Maybe Piece)
promotionParser = optionMaybe $ do
  char '='
  p <- oneOf "NBRQ"
  return $ case p of
    'N' -> Knight
    'B' -> Bishop
    'R' -> Rook
    'Q' -> Queen
    _ -> error "Impossible"

kingsideCastlingParser :: Parser Move
kingsideCastlingParser = do
  try $ string "O-O"
  eof
  return KingSideCastling

queensideCastlingParser :: Parser Move
queensideCastlingParser = do
  try $ string "O-O-O"
  eof
  return QueenSideCastling