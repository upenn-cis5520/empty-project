import Control.Concurrent.STM
import Control.Monad.State
import Lib
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"

prop_CreateInsertSelect :: String -> Bool
prop_CreateInsertSelect s =
  s
    == ( do
           execStatement $ emptyDB (StatementCreate "table" [ColumnDefinition (ColumnName "column") CellTypeString])
           execStatement $ db (StatementInsert (createRow s) (TableName "table"))
           return $ execStatement $ db' (StatementSelect [ColumnName "column"] (TableName "table") [])
       )

createRow :: String -> Row
createRow s = newTVar [CellString s]