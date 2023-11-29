module Player where

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

data Cell = Cell
  { x :: Int,
    y :: Int,
    isWall :: Bool -- true if wall is at cell
  }
  deriving (Show, Eq)

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

data Action = MUp | MDown | MLeft | MRight deriving (Eq, Show)

data Portal = Portal {entrance :: Cell, exit :: Cell} deriving (Show)

data Player = One | Two deriving (Eq, Ord, Show)

data Game = Game {board :: Maze, current :: Player, playerInfo :: PlayerInfo}

data Attributes = Attributes {numPoints :: Int, position :: Cell}

type PlayerInfo = M.Map Player Attributes

data End = Win Player | Lose Player deriving (Eq, Show)

---------------- Functions ---------------

{- Note: All functions defined under the assumption that
there cannot be multiple elements on the same cell
Ex. a portal exit & coin cannot be on same cell
this is subject to change -}

mapActionToCell :: Game -> Player -> Action -> Cell
mapActionToCell g p a = case M.lookup p (playerInfo g) of
  Just attrs ->
    case a of
      MUp -> undefined
      MDown -> undefined
      MLeft -> undefined
      MRight -> undefined
  Nothing -> undefined -- error player not found?

makeMove :: Game -> Cell -> Player -> Game
makeMove g c p =
  if c `elem` cells (board g)
    then
      if isWall c
        then error "can't move there" -- do something if trying to move to wall
        else
          if p == current g
            then
              if c == goal (board g)
                then undefined -- cur player wins, what to return? - should we use Either Monad?
                else
                  if c `elem` coins (board g)
                    then collectCoin g c p
                    else
                      if c `elem` getPortalEntrances (board g)
                        then enterPortal g c p
                        else
                          if c `elem` compasses (board g)
                            then collectCompass g c p
                            else moveOne g c p
            else undefined -- do something if player isn't current
    else undefined -- error trying to move outside of board?

getPortalEntrances :: Maze -> [Cell]
getPortalEntrances maze = map getEntrance (portals maze)
  where
    getEntrance :: Portal -> Cell
    getEntrance = entrance

moveOne :: Game -> Cell -> Player -> Game
moveOne g c p =
  case M.lookup p (playerInfo g) of
    Just attrs ->
      let newPlayerInfo = M.insert p (attrs {position = c}) (playerInfo g)
       in if p == One
            then g {playerInfo = newPlayerInfo, current = Two}
            else g {playerInfo = newPlayerInfo, current = One}
    Nothing -> undefined -- error player not found?

-- assuming coin = 1 point
collectCoin :: Game -> Cell -> Player -> Game
collectCoin g c p =
  case M.lookup p (playerInfo g) of
    Just attrs ->
      let newPlayerInfo = M.insert p (Attributes {position = c, numPoints = numPoints attrs + 1}) (playerInfo g)
       in let updatedMaze = removeCoin (board g) c
           in if p == One
                then Game {board = updatedMaze, playerInfo = newPlayerInfo, current = Two}
                else Game {board = updatedMaze, playerInfo = newPlayerInfo, current = One}
    Nothing -> undefined -- error player not found?

removeCoin :: Maze -> Cell -> Maze
removeCoin maze cell =
  let updatedCoins = filter (/= cell) (coins maze)
   in maze {coins = updatedCoins}

fetchPortal :: Game -> Cell -> Cell
fetchPortal g c =
  let myPortal = filter (\p -> entrance p == c) (portals (board g))
   in case myPortal of
        [x] -> exit x
        _ -> undefined -- error? how to handle case where != 1 portals found?

enterPortal :: Game -> Cell -> Player -> Game
enterPortal g c p = case M.lookup p (playerInfo g) of
  Just attrs ->
    let exitPortal = fetchPortal g c
     in let newPlayerInfo = M.insert p (attrs {position = exitPortal}) (playerInfo g)
         in if p == One
              then g {playerInfo = newPlayerInfo, current = Two}
              else g {playerInfo = newPlayerInfo, current = One}
  Nothing -> undefined -- error player not found?

collectCompass :: Game -> Cell -> Player -> Game
collectCompass g c p = case M.lookup p (playerInfo g) of
  Just attrs ->
    let newPosition = pointMe (board g) c p
     in let updatedMaze = removeCompass (board g) c
         in let newPlayerInfo = M.insert p (Attributes {position = c, numPoints = numPoints attrs + 1}) (playerInfo g)
             in let newGame = g {playerInfo = newPlayerInfo, board = updatedMaze}
                 in makeMove newGame c p -- we must move again according to the PointMe spell :)
  Nothing -> undefined -- error player not found?

removeCompass :: Maze -> Cell -> Maze
removeCompass maze cell =
  let updatedCompasses = filter (/= cell) (compasses maze)
   in maze {compasses = updatedCompasses}

pointMe :: Maze -> Cell -> Player -> Cell -- maybe return type could be Action instead of Cell, not sure
pointMe = undefined -- some logic to calculate the Cell that is 1 step towards goal

--------------- Tests ---------------

prop_compassTokenValidity :: Game -> Game -> Bool
prop_compassTokenValidity g1 g2 = undefined

prop_turnValidity :: Game -> Player -> Action -> Bool
prop_turnValidity g p a = p == current g

prop_ActionValidity :: Game -> Player -> Action -> Bool
prop_ActionValidity g p a = p == current g && undefined