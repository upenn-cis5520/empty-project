module GridMap where

import Graph 
import Lib (Edge, GraphMap)

import Brick

type Place = Int
-- | A map tile with an elevation and whether it is traversible
newtype Tile = Tile { place :: Place, elevation :: Float, traversible :: Bool, neighbors :: [Place] } deriving (Show, Eq)

newtype Grid = Grid {rows :: Int, cols :: Int, tiles :: [Tile]} deriving (Show, Eq)

-- | Get the tile at the given row and column in the grid
getTile :: Grid -> Int -> Int -> Tile -- row <= rows, col <= cols
getTile grid row col = tiles grid !! (row * cols grid + col)

-- | Create an edge between two tiles
edge :: Tile -> Tile -> Edge
edge t1 t2 = E (place t1) (place t2) (abs (elevation t1 - elevation t2))

-- | Turn a grid into a graph by iterating through the grid and creating edges between traversible neighbors with weight equal to the difference in elevation
gridToGraph :: Grid -> GraphMap
gridToGraph grid = map (\t -> (place t, map (edge t) (neighbors t))) (concat grid)

-- | Create a grid of traversible tiles with 0 elevation with the given dimensions
simpleGrid :: Int -> Int -> Grid
simpleGrid rows cols = Grid rows cols (map (\r -> map (\c -> Tile (r * cols + c) 0 True (adjacents rows cols (r*cols+c)) [0..cols - 1]) [0..rows - 1]))
    where
        adjacents :: Int -> Int -> Place -> [Place]
        adjacents rows cols place = filter (\p -> p >= 0 && p < rows * cols) [place - cols, place - 1, place + 1, place + cols]

-- | Get the neighbors of a tile in the grid 
getNeighbors :: Grid -> Int -> Int -> [Tile]
getNeighbors grid row col = map (\n -> getTile grid (n `div` cols grid) (n `mod` cols grid)) (neighbors (getTile grid row col))

-- | Set the neighbors of a tile in the grid
setNeighbors :: Grid -> Int -> Int -> [Tile] -> Grid


-- | 