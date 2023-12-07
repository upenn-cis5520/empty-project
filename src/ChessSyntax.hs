module ChessSyntax where

import Data.Char (toLower)
import Data.Map (Map, (!?))
import Data.Map qualified as Map

-- Using https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
-- Details the moves from Parser

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq)

data Color = White | Black deriving (Eq, Show)

data CPiece = CPiece Color Piece deriving (Eq)

type File = Char

type Rank = Int

data Square = Square File Rank deriving (Eq, Show)

newtype Promotion = Promotion (Maybe Piece) deriving (Eq, Show)

newtype Capture = Capture Bool deriving (Eq, Show)

newtype Check = Check Bool deriving (Eq, Show)

newtype Mate = Mate Bool deriving (Eq, Show)

type Board = Map Square CPiece

data Game = Game Board Color deriving (Eq, Show)

data MoveResult = Won Color | Draw | ContinueGame | InvalidPiece | InvalidDestination | InvalidCapture | InvalidCheck | InvalidMate deriving (Eq, Show)

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

instance Ord Square where
  compare (Square f1 r1) (Square f2 r2) = compare (r1, f1) (r2, f2)

instance Show Piece where
  show Pawn = "P"
  show Knight = "N"
  show Bishop = "B"
  show Rook = "R"
  show Queen = "Q"
  show King = "K"

instance Show CPiece where
  show (CPiece White p) = show p
  show (CPiece Black p) = map toLower $ show p
