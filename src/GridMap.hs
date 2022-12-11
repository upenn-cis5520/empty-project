module GridMap 
  (Place, Tile, Grid
  , getTile, gridToGraph, adjacents, simpleGrid, getNeighbors, setNeighbors
  -- , setTraversible, setElevation, setTile
  , gridTable
  , testTable, drawTable
  ) where

import Graph (Edge(E), GraphMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Table
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import Test.QuickCheck
  (Arbitrary (..),
  Gen,
  Property,
  Testable (..),
  (==>),
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
edge :: Tile -> Tile -> Edge
edge t1 t2 = E (place t1) (place t2) (abs (elevation t1 - elevation t2))

-- | Turn a grid into a graph by iterating through the grid and creating edges between traversible neighbors with weight equal to the difference in elevation
gridToGraph :: Grid -> GraphMap
gridToGraph grid = foldl (\m t -> if traversible t then Map.insert (place t) (map (edge t) (filter traversible (map (\p -> getTile grid (p `div` cols grid) (p `mod` cols grid)) (neighbors grid t)))) m else m) Map.empty (tiles grid)

-- | Get the places of the tiles adjacent to the given tile
adjacents :: Int -> Int -> Place -> [Place]
adjacents rows cols place = filter (\p -> p >= 0 && p < rows * cols) [place - cols, place - 1, place + 1, place + cols]

-- | Create a grid of traversible tiles with 0 elevation with the given dimensions
simpleGrid :: Int -> Int -> Grid
simpleGrid rows cols = Grid rows cols (map (\p -> Tile p 0 True (adjacents rows cols p)) [0..rows * cols - 1])

-- | Create a grid from a list of elevations and a list of traversibility
gridFromList :: Int -> Int -> [Float] -> [Bool] -> Grid --TODO: check that the lists are the right length
gridFromList rows cols elevations traversible = updateNeighbors $ Grid rows cols (map (\(p, e, t) -> Tile p e t (adjacents rows cols p)) (zip3 [0..rows * cols - 1] elevations traversible)) Map.empty

-- | Get the new neighbors of a tile in the grid based on traversibility
getNeighbors :: Grid -> Int -> Int -> [Place]
getNeighbors grid row col = filter (\p -> traversible (getTil`e grid (p `div` cols grid) (p `mod` cols grid))) (adjacents (rows grid) (cols grid) (row * cols grid + col))

-- | Set the neighbors of a tile in the grid
setNeighbors :: Grid -> Int -> Int -> [Place] -> Grid
setNeighbors grid row col newNeighbors = grid {neighbors = Map.insert (row * cols grid + col) newNeighbors (neighbors grid)}

-- | Update the neighbors of all tiles in the grid
updateNeighbors :: Grid -> Grid
updateNeighbors grid = grid {neighbors = Map.fromList (map (\t -> (place t, getNeighbors grid (place t `div` cols grid) (place t `mod` cols grid))) (tiles grid))}

-- | Set the traversibility of a tile in the grid, updating the neighbors of the tile and its neighbors
setTraversible :: Grid -> Int -> Int -> Bool -> Grid
setTraversible grid row col newTraversible = grid {tiles = map (\t -> 
  if place t == place (getTile grid row col) 
    then t {traversible = newTraversible} 
    else t) 
    (tiles grid), neighbors = Map.insert (row * cols grid + col) (getNeighbors grid row col) (neighbors grid)}
-- | Set the elevation of a tile in the grid
setElevation :: Grid -> Int -> Int -> Float -> Grid
setElevation grid row col newElevation = grid {tiles = map (\t -> if place t == place (getTile grid row col) then t {elevation = newElevation} else t) (tiles grid)}

-- | Set the tile at the given row and column in the grid
setTile :: Grid -> Int -> Int -> Tile -> Grid
setTile grid row col newTile = grid {tiles = map (\t -> if place t == place (getTile grid row col) then newTile else t) (tiles grid)}

testTable :: Table ()
testTable =
    surroundingBorder False $
    table [ [str "test", str "table"]
          , [str "is",    str "here"]
          ]

drawTable :: Table () -> Widget ()
drawTable table = C.center $ renderTable table

drawTile :: Tile -> String
drawTile tile = if traversible tile then show (elevation tile) else "X"

gridTable :: Grid -> Table ()
gridTable grid = table (map (\r -> map (\c -> str (drawTile (getTile grid r c))) [0..cols grid - 1]) [0..rows grid - 1])

drawGrid :: Grid -> Widget ()
drawGrid grid = C.center $ renderTable (gridTable grid)

-- Property Testing

-- | Check that the grid is the correct size
prop_gridSize :: Grid -> Bool
prop_gridSize grid = length (tiles grid) == rows grid * cols grid

-- | Check that each tile has the right place
prop_gridPlaces :: Grid -> Bool
prop_gridPlaces grid = all (\t -> place t == place (getTile grid (place t `div` cols grid) (place t `mod` cols grid))) (tiles grid)

-- | Check that the grid has the correct set of traversible neighbors for each tile
prop_gridNeighbors :: Grid -> Bool
prop_gridNeighbors grid = all (\t -> neighbors grid t == map (\p -> place (getTile grid (p `div` cols grid) (p `mod` cols grid))) (adjacents (rows grid) (cols grid) (place t))) (tiles grid)

-- | Check that the grid has the correct set of traversible neighbors for each tile after setting the traversibility of a tile
prop_gridNeighborsTraversible :: Grid -> Int -> Int -> Bool
prop_gridNeighborsTraversible grid row col = all (\t -> neighbors grid t == map (\p -> place (getTile grid (p `div` cols grid) (p `mod` cols grid))) (adjacents (rows grid) (cols grid) (place t))) (tiles (setTraversible grid row col False))

-- | Check that the graph produced by the grid has the correct number of nodes
prop_gridGraphNodes :: Grid -> Bool
prop_gridGraphNodes grid = length (gridToGraph grid) == rows grid * cols grid

-- | Check that the graph produced by the grid has the correct number of edges
prop_gridGraphEdges :: Grid -> Bool
prop_gridGraphEdges grid = all (\t -> length (gridToGraph grid Map.! place t) == length (neighbors grid t)) (tiles grid)

-- | Check that the graph produced by the grid has the correct edges
prop_gridGraphEdges2 :: Grid -> Bool
prop_gridGraphEdges2 grid = all (\t -> all (\e -> place e `elem` neighbors grid t) (gridToGraph grid Map.! place t)) (tiles grid)

-- | Check that the graph produced by the grid has the correct weights
prop_gridGraphWeights :: Grid -> Bool
prop_gridGraphWeights grid = all (\t -> all (\e -> weight e == abs (elevation t - elevation (getTile grid (place e `div` cols grid) (place e `mod` cols grid)))) (gridToGraph grid Map.! place t)) (tiles grid)

-- Arbitrary Instances

instance Arbitrary Grid where
  arbitrary = do
    rows <- choose (1, 10)
    cols <- choose (1, 10)
    elevations <- vectorOf (rows * cols) (choose (0, 10))
    traversible <- vectorOf (rows * cols) (elements [True, False])
    return (gridFromList rows cols elevations traversible)