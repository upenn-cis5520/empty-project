module ChessParser
  ( parseMoves,
    parseSingleMove,
  )
where

import ChessSyntax
import Control.Applicative (Alternative (many, (<|>)))
import Data.Maybe (fromMaybe, isJust)
import Test.HUnit (Test (TestCase, TestList), runTestTT, (~:), (~?=))
import Text.Parsec (ParseError, char, digit, eof, oneOf, option, optionMaybe, parse, spaces, string, try)
import Text.Parsec.String (Parser)

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
