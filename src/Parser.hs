module Parser
  ( parseMoves,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Data.Maybe (fromMaybe, isJust)
import Test.HUnit (Test (TestCase, TestList), runTestTT, (~:), (~?=))
import Text.Parsec (ParseError, char, digit, eof, oneOf, option, optionMaybe, parse, spaces, string, try)
import Text.Parsec.String (Parser)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Square = Square Char Int deriving (Show, Eq)

data MoveType = NormalMove | KingsideCastling | QueensideCastling deriving (Show, Eq)

data Move = Move
  { moveType :: MoveType,
    piece :: Maybe Piece,
    fromSquare :: Maybe Square, -- move from square
    toSquare :: Maybe Square, -- destination square
    promotion :: Maybe Piece,
    isCapture :: Bool,
    isCheck :: Bool,
    isCheckmate :: Bool
  }
  deriving (Show, Eq)

-- Given a space separated moves and return a list of Moves
parseMoves :: String -> Either ParseError [Move]
parseMoves = parse movesParser ""

-- Function that actaully parse a single chess move
parseSingleMove :: String -> Either ParseError Move
parseSingleMove = parse moveParser ""

test_SingleMove :: Test
test_SingleMove =
  TestList
    [ -- Test Pawn move
      fmap moveType (parseSingleMove "e7e3") ~?= Right NormalMove,
      fmap piece (parseSingleMove "e7e3") ~?= Right (Just Pawn),
      fmap fromSquare (parseSingleMove "e7e3") ~?= Right (Just (Square 'e' 7)),
      fmap toSquare (parseSingleMove "e7e3") ~?= Right (Just (Square 'e' 3)),
      fmap isCheck (parseSingleMove "e7e3") ~?= Right False,
      fmap toSquare (parseSingleMove "e7e3") ~?= Right (Just (Square 'e' 3)),
      -- Test King move
      fmap moveType (parseSingleMove "Ka1b4") ~?= Right NormalMove,
      fmap piece (parseSingleMove "Ka1b4") ~?= Right (Just King),
      fmap fromSquare (parseSingleMove "Ka1b4") ~?= Right (Just (Square 'a' 1)),
      fmap toSquare (parseSingleMove "Ka1b4") ~?= Right (Just (Square 'b' 4)),
      -- Test Castling
      fmap moveType (parseSingleMove "O-O") ~?= Right KingsideCastling,
      fmap moveType (parseSingleMove "O-O-O") ~?= Right QueensideCastling,
      -- Test Pawn Promotion
      fmap moveType (parseSingleMove "e7a8=Q") ~?= Right NormalMove,
      fmap piece (parseSingleMove "e7a8=Q") ~?= Right (Just Pawn),
      fmap fromSquare (parseSingleMove "e7a8=Q") ~?= Right Nothing,
      fmap toSquare (parseSingleMove "e7a8=Q") ~?= Right (Just (Square 'a' 8)),
      fmap promotion (parseSingleMove "e7a8=Q") ~?= Right (Just Queen),
      fmap promotion (parseSingleMove "e7a8=N") ~?= Right (Just Knight),
      fmap promotion (parseSingleMove "e7a8=R") ~?= Right (Just Rook),
      fmap promotion (parseSingleMove "e7a8=B") ~?= Right (Just Bishop)
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
    <|> normalMoveParser

normalMoveParser :: Parser Move
normalMoveParser = undefined

kingsideCastlingParser :: Parser Move
kingsideCastlingParser = undefined

queensideCastlingParser :: Parser Move
queensideCastlingParser = undefined
