module GridMap where

import Graph 
import Prelude (Edge, GraphMap)

import Brick

-- | A map tile with an elevation and whether it is traversible
newtype Tile {id :: Node, elevation :: Int, traversible :: Bool, neighbors :: [Tile]} deriving (Eq, Show)
type Col = [Tile]
type Grid = [Col]

-- | Create an edge between two tiles
edge :: Tile -> Tile -> Edge
edge t1 t2 = E (id t1) (id t2) (abs (elevation t1 - elevation t2))

-- | Turn a grid into a graph by iterating through the grid and creating edges between traversible neighbors with weight equal to the difference in elevation
gridToGraph :: Grid -> GraphMap
gridToGraph (col : cols) = Map.fromList (map (\t -> (id t, map (edge t) (filter traversible (neighbors t)))) col) ++ gridToGraph cols
gridToGraph [] = Map.empty

-- | Create a grid of tiles with the given dimensions
createGrid :: Int -> Int -> Grid
createGrid rows cols = map (\r -> map (\c -> Tile (r * cols + c) (r * cols + c) True []) [0..cols]) [0..rows]

-- | Get the neighbors of a tile in the grid
getNeighbors :: Grid -> Int -> Int -> [Tile]
getNeighbors grid row col = filter (\t -> id t /= row * length grid + col) (concat (map (\r -> map (\c -> grid !! r !! c) [col - 1..col + 1]) [row - 1..row + 1]))

-- | Set the neighbors of a tile in the grid
setNeighbors :: Grid -> Int -> Int -> [Tile] -> Grid
setNeighbors grid row col neighbors = map (\r -> map (\c -> if r == row && c == col then Tile (r * length grid + c) (r * length grid + c) True neighbors else grid !! r !! c) [0..length grid - 1]) [0..length grid - 1]

-- | 