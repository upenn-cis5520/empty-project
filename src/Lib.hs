module Lib
    ( someFunc
    ) where

someFunc :: String
someFunc = "Hello CIS 5520"



import Data.Map (Map)
import qualified Data.Map as Map

type Node = Int
-- data Graph = G Vertices Edges deriving Show
-- newtype Vertices = V [Node]
data Edge = E {source::Node, dest:: Node, weight::Int}
-- type Edges = [Edge]
type GraphMap = Map Node [Edge]
type Path = [Edge]
type PathMap = Map Node Path
type SingleSource = (GraphMap -> Node -> PathMap)

toAdjList :: Graph -> GraphMap

dijkstra :: GraphMap -> Node -> PathMap

bellmanFord :: GraphMap -> Node -> PathMap

astar :: GraphMap -> Node -> Node -> PathMap

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

-- Tests whether a path-finding algorithm correctly finds no path
-- between two nodes that are not connected. Iterates over all
-- 
testUnconnected :: GraphMap -> Node -> PathMap -> Bool
testUnconnected g s m = all good (Map.keys g)
    where
        good d = unconnected g s d == not (Map.member d m)
        
-- Arbitrary Instances