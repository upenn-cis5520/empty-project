module Graph
    ( Node, Edge(E), GraphMap, Path, 
    ) where
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS.Lazy (RWS, runRWS, evalRWS, ask, tell, get, put, MonadReader, MonadWriter, MonadState)
import qualified Data.Text as T


data Node = N {name:: String, getX :: Float, getY :: Float} deriving (Show, Eq)
-- data Graph = G Vertices Edges deriving Show 
-- newtype Vertices = V [Node]
data Edge a = E {source::Node, dest:: Node, weight::a} deriving (Show, Eq)
-- type Edges = [Edge]
type GraphMap a = Map Node [Edge a]
type Path  = [Edge Float]



-- describe general heuristic 
class Heuristic a where
    euclid_distance  :: a -> a -> Float
    manhattan_distance :: a -> a -> Float


instance Heuristic Node where
    euclid_distance :: Node -> Node -> Float
    euclid_distance f g = sqrt ((getY g - getY f) * (getY g - getY f)  + (getX g - getX f) * (getX g - getX f))



-- Define traversible as things we can define an edge over
class Traversible a where 
    mkEdge :: (Num a, Ord a) => Node -> Node -> a -> Edge a


instance Traversible Float where
    mkEdge :: Node -> Node -> Float -> Edge Float
    mkEdge = E 
    
    

data SPConfig = SPConfig {
    graph :: GraphMap Float,
    startNodes :: [Node],
    endNode :: [Node],
    hDist :: [Float]
} 

data OrderSnapshot = Snapshot {
    current :: Node,
    -- Explored nodes
    expNodes :: [Node],
    -- Explored edges 
    expEdges :: Path,
    -- Shortest path
    spPath :: Path,
    -- Best estimation path length (A*)
    bestEstim :: Float, 
    -- Graph 
    graph :: GraphMap Float
} deriving (Show)



-- reader shouldn't be start state, config file 
-- make rendering datatype 
newtype SPContext st = SP { context :: RWS SPConfig (T.Text, OrderSnapshot) OrderSnapshot st } deriving (Monad, 
                                                                        Functor, 
                                                                        Applicative, 
                                                                        MonadReader (SPConfig), 
                                                                        MonadWriter (T.Text, OrderSnapshot), 
                                                                        MonadState OrderSnapshot)




dfsStep ::  OrderSnapshot -> SPContext (OrderSnapshot)
dfsStep = undefined
    








-- Use monad transformers with a writer monad, as hardwired state monad is hard to change
-- Store snapshots of intermediate states
-- Look into graph module for generating well formed graphs for quickcheck
-- {Can use refinement types using LiquidHaskell}
-- Look into visualization using Gloss
-- Write type class interface for top level operations (like reading and writing to json)
-- More flexible node type class for heuristic computation 

-- | Returns the sum of edge weights in a path
pathLength :: Path -> Float
pathLength = sum . map weight

isCircular :: Path -> Bool
isCircular = traverse [] where 
    traverse visited [] =  False
    traverse visited (e:es) =  (source e `elem` visited) || traverse (source e :visited) es 



-- Unit Tests


        
-- Arbitrary Instances
