
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
type PathMap = Map Node Path
type SingleSource = (GraphMap -> Node -> PathMap)

--toAdjList :: Graph -> GraphMap


type Visited = S Store Bool 
type Distances = S Store Bool 

queryVisited :: (Store -> Bool) -> Visited
queryVisited = undefined 

queryDistance :: (Store -> Float) -> Distances
queryDistance = undefined


-- Non-negative graph map
data NN = O | I Float
data NNE src dest weight where
    W :: NNE src dest O
    NNE :: src -> dest -> weight -> NNE src dest (I weight)

<<<<<<< HEAD
dijkstra :: GraphMap -> Node -> PathMap
=======
type GraphMapNN = Map Node [NNE]

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
>>>>>>> 04d61e389c10ffd33b9adf78e06878d20f653644

bellmanFord :: GraphMap -> Node -> PathMap

<<<<<<< HEAD
astar :: GraphMap -> Node -> Node -> PathMap
=======


dfs :: GraphMap -> Node -> Map Node Path
>>>>>>> 04d61e389c10ffd33b9adf78e06878d20f653644

bfs :: GraphMap -> Node -> PathMap

-- | Returns true if there is no path from the source to the destination
unconnected :: GraphMap -> Node -> Node -> Bool
unconnected g s d = not $ Map.member d $ bfs g s

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
pathShortest :: GraphMap -> Node -> Node -> Path -> Bool
pathShortest g s d p = pathValid g p && (length p == length (bellmanFord g s Map.! d))
-- Check pathValid and pathShortest for all shortest paths from a single node
allShortest :: GraphMap -> Node -> PathMap-> Bool
allShortest g s m = all (uncurry (pathShortest g s)) (Map.toList m)
-- Unit Tests

<<<<<<< HEAD
-- Tests whether a path-finding algorithm correctly finds no path
-- between two nodes that are not connected. Iterates over all
-- 
testUnconnected :: GraphMap -> Node -> PathMap -> Bool
testUnconnected g s m = all good (Map.keys g)
    where
        good d = unconnected g s d == not (Map.member d m)
        
-- Arbitrary Instances
=======


>>>>>>> 04d61e389c10ffd33b9adf78e06878d20f653644
