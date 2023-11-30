module Parser
  ( parseMoves,
    parseSingleMove,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Data.Maybe (fromMaybe, isJust)
import Test.HUnit (Test (TestCase, TestList), runTestTT, (~:), (~?=))
import Text.Parsec (ParseError, char, digit, eof, oneOf, option, optionMaybe, parse, spaces, string, try)
import Text.Parsec.String (Parser)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)

data Color = White | Black deriving (Eq, Show)

type File = Char

type Rank = Int

data Square = Square File Rank deriving (Eq, Show)

newtype Promotion = Promotion (Maybe Piece) deriving (Eq, Show)

newtype Capture = Capture Bool deriving (Eq, Show)

newtype Check = Check Bool deriving (Eq, Show)

newtype Mate = Mate Bool deriving (Eq, Show)

-- the square is the destination
data Move
  = NormalMove Piece Square (Maybe Disambiguation) Promotion Capture Check Mate
  | KingSideCastling
  | QueenSideCastling
  deriving (Eq, Show)

data Disambiguation
  = File File
  | Rank Rank
  | Both Square
  deriving (Show, Eq)

-- Using https://en.wikipedia.org/wiki/Algebraic_notation_(chess) for moves
-- Given pace separated moves, return a list of Moves
parseMoves :: String -> Either ParseError [Move]
parseMoves = parse movesParser ""

-- Function that actaully parse a single chess move
parseSingleMove :: String -> Either ParseError Move
parseSingleMove = parse moveParser ""

test_SingleMove :: Test
test_SingleMove =
  TestList
    [ -- Test Pawn move
      parseSingleMove "e3" ~?= Right (NormalMove Pawn (Square 'e' 3) Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False)),
      -- Test Pawn move with disambiguation
      parseSingleMove "e7e3" ~?= Right (NormalMove Pawn (Square 'e' 3) (Just (Both (Square 'e' 7))) (Promotion Nothing) (Capture False) (Check False) (Mate False)),
      -- Test King move
      parseSingleMove "Ka1b4" ~?= Right (NormalMove King (Square 'a' 1) (Just (Both (Square 'b' 4))) (Promotion Nothing) (Capture False) (Check False) (Mate False)),
      -- Test Castling
      parseSingleMove "O-O" ~?= Right KingSideCastling,
      parseSingleMove "O-O-O" ~?= Right QueenSideCastling,
      -- Test Pawn Promotion
      parseSingleMove "e7a=Q" ~?= Right (NormalMove Pawn (Square 'e' 7) (Just (File 'a')) (Promotion (Just Queen)) (Capture False) (Check False) (Mate False))
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
