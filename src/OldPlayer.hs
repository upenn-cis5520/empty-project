module OldPlayer where

import Control.Monad (when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import State (State)
import State qualified as S
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)

data Location = Loc Int Int deriving (Eq, Ord, Show)

data Player = One | Two deriving (Eq, Ord, Show)

-- type Maze = M.Map Location Player

type Maze = M.Map Location Value

type GameState = State Maze Player

data Action = Up | Down | Left | Right deriving (Eq, Show)

data Portal = EntranceExit {entrance :: Location, exit :: Location}

data Value = Start Location | Coin Int | Portal Portal | Goal Location | Position Int | Compass Location

-- data Game = Game {board :: Board, current :: Player} deriving (Eq, Show)

data Game = Game {board :: Maze, current :: Player, playerInfo :: PlayerInfo}

type PlayerInfo = M.Map Player Attributes

data Attributes = Attributes {numPoints :: Int, position :: Location}

data End = Win Player | Lose Player deriving (Eq, Show)

data Cell = Cell
  { x :: Int,
    y :: Int,
    isWall :: Bool
  }
  deriving (Show)

data Maze = Maze
  { cells :: [Cell],
    startPlayerOne :: Cell,
    startPlayerTwo :: Cell,
    goal :: Cell,
    coins :: [Cell],
    compasses :: [Cell],
    portals :: [Portal]
  }
  deriving (Show)

data Game = Game {board :: Maze, current :: Player, playerInfo :: PlayerInfo}

data Attributes = Attributes {numPoints :: Int, position :: Location}

type PlayerInfo = M.Map Player Attributes

data Name = One | Two

data Player = Player {
  name :: Name
  position :: Cell,
  tools :: [Attributes]
}

-- data Portal = Portal Cell Cell deriving (Show)
data Portal = Portal {entrance :: Cell, exit :: Cell} deriving (Show)

-- makeMove :: Location -> Player -> GameState -> ()
makeMove :: Game -> Location -> Player -> Game
makeMove g l@(Loc x y) p =
  case M.lookup l (board g) of
    Nothing -> error "something went wrong"
    Just v -> case v of
      (Start s) -> undefined -- how should we handle? should be similar to Position case?
      (Coin c) -> collectCoin g l p
      (Compass comp) -> collectCompass g l p
      (Portal port) -> enterPortal g l p
      (Goal goal) -> error "Game Completed!" -- do something to indicate game done
      (Position i) -> case i of
        0 -> g -- do some prompting or indicate that player must choose a diff direction
        1 ->
          if p == current g
            then case M.lookup p (playerInfo g) of
              Just attrs -> do
                if position attrs == l
                  then undefined -- do some prompting to indicate player must choose different location?
                  else
                    if p == Two
                      then Game {board = board g, current = One, playerInfo = M.insert p Attributes {numPoints = numPoints attrs, position = l} (playerInfo g)}
                      else Game {board = board g, current = Two, playerInfo = M.insert p Attributes {numPoints = numPoints attrs, position = l} (playerInfo g)}
              Nothing -> g
            else g
        _ -> error "invalid move"

-- collectCoin :: Location -> Player -> GameState -> ()
collectCoin :: Game -> Location -> Player -> Game
collectCoin g l@(Loc x y) p =
  case M.lookup l (board g) of
    Nothing -> error "something went wrong"
    Just v -> case v of
      (Coin c) ->
        if p == current g
          then case M.lookup p (playerInfo g) of
            Just attrs -> do
              let pts = c + numPoints attrs
               in if p == Two
                    then Game {board = M.insert l (Position 1) (board g), current = One, playerInfo = M.insert p Attributes {numPoints = pts, position = l} (playerInfo g)}
                    else Game {board = M.insert l (Position 1) (board g), current = Two, playerInfo = M.insert p Attributes {numPoints = pts, position = l} (playerInfo g)}
            _ -> g
          else g
      _ -> g

-- enterPortal :: Location -> Player -> GameState -> ()
enterPortal :: Game -> Location -> Player -> Game
enterPortal g l@(Loc x y) p =
  case M.lookup l (board g) of
    Nothing -> error "something went wrong"
    Just v -> case v of
      (Portal port) ->
        if p == current g
          then case M.lookup p (playerInfo g) of
            Just attrs -> do
              let newPos = exit port
               in if p == Two
                    then Game {board = board g, current = One, playerInfo = M.insert p Attributes {numPoints = numPoints attrs, position = newPos} (playerInfo g)}
                    else Game {board = board g, current = Two, playerInfo = M.insert p Attributes {numPoints = numPoints attrs, position = newPos} (playerInfo g)}
            Nothing -> g
          else g
      _ -> g

-- collectCompass :: Location -> Player -> GameState -> ()
collectCompass :: Game -> Location -> Player -> Game
collectCompass g l@(Loc x y) p =
  case M.lookup l (board g) of
    Nothing -> error "something went wrong"
    Just v -> case v of
      (Compass c) ->
        if p == current g
          then case M.lookup p (playerInfo g) of
            Just attrs -> do
              let newPos = c
               in if p == Two
                    then Game {board = board g, current = One, playerInfo = M.insert p Attributes {numPoints = numPoints attrs, position = newPos} (playerInfo g)}
                    else Game {board = board g, current = Two, playerInfo = M.insert p Attributes {numPoints = numPoints attrs, position = newPos} (playerInfo g)}
            Nothing -> g
          else g
      _ -> g

-- prop_compassTokenValidity :: GameState -> GameState -> Bool
prop_compassTokenValidity :: Game -> Game -> Bool
prop_compassTokenValidity g1 g2 = undefined

-- prop_turnValidity :: GameState -> Player -> Action -> Bool
prop_turnValidity :: Game -> Player -> Action -> Bool
prop_turnValidity g p a = p == current g

prop_ActionValidity :: Game -> Player -> Action -> Bool
prop_ActionValidity g p a = p == current g && undefined