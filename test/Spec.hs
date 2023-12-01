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
    == evalState
      ( do
          execStatement $ StatementCreate "table" [ColumnDefinition (ColumnName "column") String]
          execStatement $ StatementInsert (createRow s) (TableName "table")
          return $ execStatement $ StatementSelect [ColumnName "column"] (TableName "table") []
      )

createRow :: String -> Row
createRow s = [newTVar s]