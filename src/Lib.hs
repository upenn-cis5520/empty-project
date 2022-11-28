module Lib
    ( someFunc
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

type Node = Int
-- data Graph = G Vertices Edges deriving Show
-- newtype Vertices = V [Node]
data Edge = E {source::Node, dest:: Node, weight::Int}
-- type Edges = [Edge]
type GraphMap = Map Node [Edge]
type Path = [Edge]

toAdjList :: Graph -> GraphMap

dijkstra :: GraphMap -> Node -> Map Node Path

bfs :: GraphMap -> Node -> Map Node Path

dfs :: GraphMap -> Node -> Map Node Path




