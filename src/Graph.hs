module Graph
    ( Node, Edge(E), GraphMap, Path, 
    ) where
import Data.Map (Map)
import qualified Data.Map as Map

type Node = Int
-- data Graph = G Vertices Edges deriving Show
-- newtype Vertices = V [Node]
data Edge = E {source::Node, dest:: Node, weight::Float}
-- type Edges = [Edge]
type GraphMap = Map Node [Edge]
type Path = [Edge]


-- | Returns the sum of edge weights in a path
pathLength :: Path -> Float
pathLength = sum . map weight





-- Unit Tests


        
-- Arbitrary Instances
