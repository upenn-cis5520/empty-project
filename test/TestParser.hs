import Parser
import Test.HUnit
  ( Counts,
    Test (TestCase, TestList),
    assertEqual,
    runTestTT,
  )
import Types

-- Define some test cases
testParseSQL :: Test
testParseSQL = TestList [testSelect, testInsert, testDrop]

testSelect :: Test
testSelect = TestCase $ do
  let sql = "SELECT id, name FROM users WHERE id = 1"
  let expected = Right (StatementSelect [ColumnName "id", ColumnName "name"] (TableName "users") [WhereClause (ColumnName "id") (CellInt 1)])
  assertEqual "Testing SELECT parsing" expected (parseSQL sql)

testInsert :: Test
testInsert = TestCase $ do
  let sql = "INSERT INTO users VALUES (1, 'John Doe')"
  let expected = Right (StatementInsert (TVar [CellInt 1, CellString "John Doe"]) (TableName "users"))
  assertEqual "Testing INSERT parsing" expected (parseSQL sql)

testDrop :: Test
testDrop = TestCase $ do
  let sql = "DROP TABLE users"
  let expected = Right (StatementDrop (TableName "users"))
  assertEqual "Testing DROP parsing" expected (parseSQL sql)

-- Function to run all tests
runTests :: IO Counts
runTests = runTestTT testParseSQL