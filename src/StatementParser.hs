import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Functor (($>))
import Parser (Parser)
import Parser qualified as P
import Test.HUnit
import Types

alphaNum :: Parser Char
alphaNum = P.satisfy isAlphaNum

-- Basic parsers
identifier :: Parser String
identifier = some P.alpha <|> P.char '_' *> many alphaNum

parsedTableName :: Parser TableName
parsedTableName = TableName <$> identifier

columnName :: Parser ColumnName
columnName = ColumnName <$> identifier

parsedCell :: Parser Cell
parsedCell = P.choice [cellInt, cellString, cellBool]

cellInt :: Parser Cell
cellInt = CellInt <$> P.int

cellString :: Parser Cell
cellString = CellString <$> (P.char '"' *> some (P.satisfy (/= '"')) <* P.char '"')

cellBool :: Parser Cell
cellBool = CellBool <$> (true <|> false)
  where
    true = P.string "TRUE" $> True
    false = P.string "FALSE" $> False

whereClause :: Parser WhereClause
whereClause =
  WhereClause
    <$> columnName
    <*> (P.space *> parsedCell)

selectParser :: Parser Statement
selectParser = StatementSelect <$> (P.string "SELECT" *> P.space *> P.sepBy columnName (P.char ',' <* P.space)) <*> (P.string "FROM" *> P.space *> parsedTableName) <*> (P.string "WHERE" *> P.space *> P.sepBy whereClause (P.space *> P.string "AND" <* P.space))

insertParser :: Parser Statement
insertParser =
  liftA2
    (flip StatementInsert)
    (P.string "INSERT INTO" *> P.space *> parsedTableName <* P.space <* P.string "VALUES" <* P.space)
    (TVar <$> P.sepBy parsedCell (P.char ',' <* P.space))

dropParser :: Parser Statement
dropParser = StatementDrop <$> (P.string "DROP TABLE" *> P.space *> parsedTableName)

dropIndexParser :: Parser Statement
dropIndexParser = StatementDropIndex <$> (P.string "DROP INDEX" *> P.space *> indexName)
  where
    indexName = IndexName <$> identifier

createParser :: Parser Statement
createParser = StatementCreate <$> (P.string "CREATE TABLE" *> P.space *> parsedTableName) <*> (P.char '(' *> P.space *> P.sepBy columnDefinition (P.char ',' <* P.space) <* P.char ')')
  where
    columnDefinition = ColumnDefinition <$> columnName <*> (P.space *> cellType)
    cellType = CellTypeInt <$ P.string "INT" <|> CellTypeString <$ P.string "STRING" <|> CellTypeBool <$ P.string "BOOL"

createIndexParser :: Parser Statement
createIndexParser = StatementCreateIndex <$> (P.string "CREATE INDEX" *> P.space *> indexName) <*> (P.string "ON" *> P.space *> parsedTableName) <*> (P.char '(' *> P.space *> P.sepBy columnName (P.char ',' <* P.space) <* P.char ')')
  where
    indexName = IndexName <$> identifier

-- Combine all statement parsers
statementParser :: Parser Statement
statementParser = P.choice [selectParser, insertParser, dropParser, dropIndexParser, createParser, createIndexParser]

-- Example of how to use the parser
parseSQL :: String -> Either P.ParseError Statement
parseSQL = P.parse statementParser

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