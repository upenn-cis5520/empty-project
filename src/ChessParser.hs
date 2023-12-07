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
    <|> try normalMoveWithDisambiguationParser
    <|> normalMoveParser

normalMoveParser :: Parser Move
normalMoveParser = do
  p <- optionMaybe pieceParser
  capture <- captureParser
  toSquare <- squareParser
  prom <- promotionParser
  check <- optionMaybe (char '+')
  checkmate <- optionMaybe (char '#')
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
  return $ Square (read [rank]) file

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
  return $ Both $ Square (read [rank]) file

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
  return KingSideCastling

queensideCastlingParser :: Parser Move
queensideCastlingParser = do
  try $ string "O-O-O"
  return QueenSideCastling
