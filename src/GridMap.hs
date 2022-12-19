module GridMap 
  (Place, Tile(..), Grid(..)
  , getTile, gridToGraph, adjacents, simpleGrid, getNeighbors, setNeighbors
  , setTraversible, setElevation, setTile
  , gridFromList
  , testGridMap
  ) where

import Graph (Edge(E), GraphMap)
import Data.Map (Map)
import qualified Data.Map as Map

import Test.QuickCheck
  (Arbitrary (..),
  Gen,
  Property,
  Testable (..),
  (==>),
  quickCheck
  )
import qualified Test.QuickCheck as QC

-- | Identifier for tiles in a grid
type Place = Int
-- | A map tile with an elevation, whether it is traversible
data Tile = Tile { place :: Place, elevation :: Float, traversible :: Bool} deriving (Show, Eq)

-- | A grid of tiles with a given number of rows and columns, 
data Grid = Grid {rows :: Int, cols :: Int, tiles :: [Tile], neighbors :: Map Place [Place]} deriving (Show, Eq)

-- | Get the tile at the given row and column in the grid
getTile :: Grid -> Int -> Int -> Tile -- row <= rows, col <= cols
getTile grid row col = tiles grid !! (row * cols grid + col)

-- | Create an edge between two tiles
edge :: Tile -> Tile -> Edge Float
edge t1 t2 = E (place t1) (place t2) (abs (elevation t1 - elevation t2))

-- | Turn a grid into a graph by iterating through the grid and creating edges between traversible neighbors with weight equal to the difference in elevation
gridToGraph :: Grid -> GraphMap Float
gridToGraph grid = Map.fromList (map (\t -> (place t, map (edge t) (map (getTile grid (place t `div` cols grid)) (neighbors grid Map.! place t)))) (tiles grid))

-- | Get the places of the tiles adjacent to the given tile
adjacents :: Int -> Int -> Place -> [Place]
adjacents rows cols place = filter (\p -> p >= 0 && p < rows * cols) [place - cols, place - 1, place + 1, place + cols]

-- | Create a grid of traversible tiles with 0 elevation with the given dimensions
simpleGrid :: Int -> Int -> Grid
simpleGrid rows cols = updateNeighbors $ Grid rows cols (map (\p -> Tile p 0 True) [0..rows * cols - 1]) Map.empty

-- | Create a grid from a list of elevations and a list of traversibility
gridFromList :: Int -> Int -> [Float] -> [Bool] -> Grid --TODO: check that the lists are the right length
gridFromList rows cols elevations traversible = updateNeighbors $ Grid rows cols (map (\p -> Tile p (elevations !! p) (traversible !! p)) [0..rows * cols - 1]) Map.empty

-- | Get the new neighbors of a tile in the grid based on traversibility
getNeighbors :: Grid -> Int -> Int -> [Place]
getNeighbors grid row col = filter (\p -> traversible (getTile grid (p `div` cols grid) (p `mod` cols grid))) (adjacents (rows grid) (cols grid) (row * cols grid + col))

-- | Set the neighbors of a tile in the grid
setNeighbors :: Grid -> Int -> Int -> [Place] -> Grid
setNeighbors grid row col newNeighbors = grid {neighbors = Map.insert (row * cols grid + col) newNeighbors (neighbors grid)}

-- | Update the neighbors of all tiles in the grid
updateNeighbors :: Grid -> Grid
updateNeighbors grid = grid {neighbors = Map.fromList (map (\t -> (place t, getNeighbors grid (place t `div` cols grid) (place t `mod` cols grid))) (tiles grid))}

-- | Set the traversibility of a tile in the grid, updating the neighbors of the tile and its neighbors
setTraversible :: Grid -> Int -> Int -> Bool -> Grid
setTraversible grid row col newTraversible = updateNeighbors $ grid {tiles = map (\t -> if place t == place (getTile grid row col) then t {traversible = newTraversible} else t) (tiles grid)}
-- | Set the elevation of a tile in the grid
setElevation :: Grid -> Int -> Int -> Float -> Grid
setElevation grid row col newElevation = grid {tiles = map (\t -> if place t == place (getTile grid row col) then t {elevation = newElevation} else t) (tiles grid)}

-- | Set the tile at the given row and column in the grid
setTile :: Grid -> Int -> Int -> Tile -> Grid
setTile grid row col newTile = grid {tiles = map (\t -> if place t == place (getTile grid row col) then newTile else t) (tiles grid)}

-- | Return whether the tile at the given row and column is traversible
isTraversible :: Grid -> Int -> Int -> Bool
isTraversible grid row col = traversible (getTile grid row col)

-- | Same as isTraversible, using a Place instead of a row and column
placeTraversible :: Grid -> Place -> Bool
placeTraversible grid place = traversible (getTile grid (place `div` cols grid) (place `mod` cols grid))

-- | Return the id (place) of the tile at the given row and column
getPlace :: Grid -> Int -> Int -> Place
getPlace grid row col = place (getTile grid row col)


-- | Check whether the given edge is between two traversible tiles and has the correct weight
edgeValid :: Grid -> Edge Float -> Bool
edgeValid grid (E p1 p2 w) = placeTraversible grid p2 && w == abs (elevation (getTile grid (p1 `div` cols grid) (p1 `mod` cols grid)) - elevation (getTile grid (p2 `div` cols grid) (p2 `mod` cols grid)))



-- Property Testing

-- | Check that the grid is the correct size
prop_gridSize :: Grid -> Bool
prop_gridSize grid = length (tiles grid) == rows grid * cols grid

-- | Check that each tile has the right place
prop_gridPlaces :: Grid -> Bool
prop_gridPlaces grid = all (\t -> place t == place (getTile grid (place t `div` cols grid) (place t `mod` cols grid))) (tiles grid)

-- | Check that the grid has the correct set of traversible neighbors for each tile
prop_gridNeighbors :: Grid -> Bool
prop_gridNeighbors grid = all (\t -> (neighbors grid Map.! place t) == (getNeighbors grid (place t `div` cols grid) (place t `mod` cols grid))) (tiles grid)

-- | Check that the graph produced by the grid has the correct number of nodes
prop_gridGraphNodes :: Grid -> Bool
prop_gridGraphNodes grid = length (gridToGraph grid) == rows grid * cols grid

-- | Check that the graph produced by the grid has the correct number of edges
prop_gridGraphEdges :: Grid -> Bool
prop_gridGraphEdges grid = all (\t -> (length (gridToGraph grid Map.! place t)) == (length $ ((neighbors grid) Map.! (place t)))) (tiles grid)


-- | Check that each edge is between two traversible tiles and has the correct weight
prop_gridGraphEdges2 :: Grid -> Bool
prop_gridGraphEdges2 grid = all (edgeValid grid) (concatMap (\t -> map (\p -> E (place t) p (abs (elevation t - elevation (getTile grid (p `div` cols grid) (p `mod` cols grid))))) (neighbors grid Map.! place t)) (tiles grid))

-- Arbitrary Instances

instance Arbitrary Grid where
  arbitrary = do
    rows <- QC.choose (1, 10)
    cols <- QC.choose (1, 10)
    elevations <- QC.vectorOf (rows * cols) (QC.choose (0, 10))
    traversible <- QC.vectorOf (rows * cols) (QC.elements [True, False])
    return (gridFromList rows cols elevations traversible)
  shrink grid = [simpleGrid (rows grid) (cols grid)]

testGridMap :: IO ()
testGridMap = do
  quickCheck prop_gridSize
  quickCheck prop_gridPlaces
  quickCheck prop_gridNeighbors
  quickCheck prop_gridGraphNodes
  quickCheck prop_gridGraphEdges
  quickCheck prop_gridGraphEdges2