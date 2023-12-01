module ChessSyntax where

import Data.Map (Map, (!?))
import Data.Map qualified as Map

-- Using https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
-- Details the moves from Parser

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)

data Color = White | Black deriving (Eq, Show)

type File = Char

type Rank = Int

data Square = Square File Rank deriving (Eq, Show)

newtype Promotion = Promotion (Maybe Piece) deriving (Eq, Show)

newtype Capture = Capture Bool deriving (Eq, Show)

newtype Check = Check Bool deriving (Eq, Show)

newtype Mate = Mate Bool deriving (Eq, Show)

type Board = Map Square Piece

data Game = Game Board Color deriving (Eq, Show)

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

