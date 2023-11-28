module LuTypes where


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