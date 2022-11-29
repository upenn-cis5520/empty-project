module Lib
    ( someFunc
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified State as S

type Node = Int
-- data Graph = G Vertices Edges deriving Show
-- newtype Vertices = V [Node]
data Edge = E {source::Node, dest:: Node, weight::Float}
-- type Edges = [Edge]
type GraphMap = Map Node [Edge]
type Path = [Edge]

--toAdjList :: Graph -> GraphMap


type Visited = S Store Bool 
type Distances = S Store Bool 

queryVisited :: (Store -> Bool) -> Visited
queryVisited = undefined 

queryDistance :: (Store -> Float) -> Distances
queryDistance = undefined


-- Non-negative graph map


dijkstra :: GraphMap -> Node -> Map Node Path
dijkstra = undefined
    where 
        search :: GraphMap -> Node -> Visited -> Distances
        search = undefined
        searchNeighbors :: GraphMap -> [Node] -> Visited -> Distances
        searchNeighbors = undefined 


AStar :: GraphMap -> Node -> Map Node Path
AStar = undefined
    where 
        search :: GraphMap -> Node -> Visited -> Distances
        search = undefined
        searchNeighbors :: GraphMap -> [Node] -> Visited -> Distances
        searchNeighbors = undefined 
    
-- Dijkstra only takes weighted graph without cycle

bfs :: GraphMap -> Node -> Map Node Path



dfs :: GraphMap -> Node -> Map Node Path






