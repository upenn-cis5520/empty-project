module LuTypeCheck where

import LuSyntax
import State (State)
import Data.Map (Map)
-- Potentially add Unknown, Any as we see fit. 
data LType = 
    Never 
    | NilType
    | IntType
    | BooleanType
    | TableType LType LType -- What about a table as a key??
    | UnionType LType LType 
    | FunctionType LType LType -- Partial Function 

isValueType :: LType -> Bool 
isValueType = undefined

{- TODO: Formalize this into test. (want to reject)
x = "here "

x = 5 
-}

{- TODO: Formalize this into test. (want to reject)
k = "x"
a[k] = 10      
a[20] = "great"
-}

{-TODO: test for union types with table of two different keys. 

-}

{-quickCheck tests 
- synthesis(evaluated statement) == synthesis(original statement)
- checker(synthesis(statement), statement) == True 
- 

-}

type EnvironmentTypes = Map Name LType
type EnvironmentState = State EnvironmentTypes ()

-- | Given a block and an environment, check if the types are consistent in the block. 
typeCheckBlock :: Block -> EnvironmentState -> State EnvironmentTypes ()
typeCheckBlock = undefined

-- | Given a statement and an environment, check if the types are consistent in the statement. 
typeCheckStatement :: Statement -> EnvironmentState -> State EnvironmentTypes () 
typeCheckStatement = undefined

-- | Determine type of a given expression and with environment. 
synthesis :: Expression -> EnvironmentTypes -> LType
synthesis = undefined

-- | Check that type of given expression is same as given type. 
checker :: Expression -> LType -> EnvironmentTypes -> Bool
checker = undefined