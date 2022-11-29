
{-#LANGUAGE GADTs, EmptyDataDecls #-}

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
data NN = O | S NN -- use peano numbers to get non-negative
data NNE :: Type -> Type -> NN -> Type where
    NNE :: src -> dest -> weight -> NNE src dest weight 



type family IsNN (src :: Node ) (dest :: Node) (weight :: Int) :: Bool  where 
    IsNN src dest w = w > 0

type GraphMapNN = Map Node [NNE]


--check if a graph is a valid (directed acylic graph)
isValidDAG :: GraphMapNN -> Bool
isValidDAG = undefined


dijkstra :: GraphMapNN -> Node -> Map Node Path
dijkstra = undefined
    where 
        search :: GraphMapNN-> Node -> Visited -> Distances
        search = undefined
        searchNeighbors :: GraphMapNN -> [Node] -> Visited -> Distances
        searchNeighbors = undefined 


aStar :: GraphMap -> Node -> Map Node Path
aStar = undefined
    where 
        search :: GraphMap -> Node -> Visited -> Distances
        search = undefined
        searchNeighbors :: GraphMap -> [Node] -> Visited -> Distances
        searchNeighbors = undefined 

    
-- Dijkstra only takes weighted graph without cycle


bellmanFord :: GraphMapNN -> Node -> Map Node Path
bellmanFord = undefined
    where 
        search :: GraphMapNN-> Node -> Visited -> Distances
        search = undefined
        searchNeighbors :: GraphMapNN -> [Node] -> Visited -> Distances
        searchNeighbors = undefined 

bfs :: GraphMap -> Node -> Map Node Path



dfs :: GraphMap -> Node -> Map Node Path


-- Given a graph (adjacency list) and path (list of nodes), return whether
-- the path is a valid path in the graph.
pathValid :: GraphMap -> Path -> Bool
pathValid g p = all (inGraph g) p && isValid p
    where
        inGraph g (E s d _) = d `elem` (g Map.! s)
        isValid [] = True
        isValid [_] = True
        isValid (E s1 d1 _ : E s2 d2 _ : ps) = d1 == s2 && isValid (E s2 d2 _ : ps)

-- Checks whether a path is a shortest path by comparing with a known algorithm.
pathShortest :: Graph -> Node -> Node -> Path -> Bool


-- Unit Tests
-- Tests whether a path-finding algorithm returns a direct path from start
-- to end when available.
directPathValid :: PathFinder -> Graph -> Node -> Node -> Bool 
-- add edge from start to end, 

-- Tests whether a path-finding algorithm correctly returns Nothing when
-- there is no path from start to end.
testUnconnected :: PathFinder -> Graph -> Node -> Node -> Bool



