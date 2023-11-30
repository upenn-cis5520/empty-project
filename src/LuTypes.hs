module LuTypes where
import Test.QuickCheck (Arbitrary (..), Gen)



-- Potentially add Unknown, Any as we see fit. 
data LType = 
    Never 
    | NilType
    | IntType
    | StringType
    | BooleanType
    | TableType LType LType -- What about a table as a key??
    | UnionType LType LType 
    | FunctionType LType LType -- Partial Function 
    deriving (Eq, Show)

instance Arbitrary LType where
    arbitrary :: Gen LType
    arbitrary = undefined

    shrink :: LType -> [LType]
    shrink = undefined