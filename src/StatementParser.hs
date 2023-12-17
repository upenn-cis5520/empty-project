module StatementParser where

import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Functor (($>))
import Parser (Parser)
import Parser qualified as P
import Test.HUnit
import Types

import Data.Map as Map

alphaNum :: Parser Char
alphaNum = P.satisfy isAlphaNum

-- Basic parsers
identifier :: Parser String
identifier = some P.alpha <|> P.char '_' *> many alphaNum

parsedTableName :: Parser TableName
parsedTableName = TableName <$> identifier

columnName :: Parser ColumnName
columnName = ColumnName <$> identifier

test_columnName :: Test
test_columnName =
  TestList
    [ 
      P.parse columnName "Users" ~?= Right (ColumnName "Users"),
      P.parse columnName "id, name" ~?= Right (ColumnName "id")
    ]

-- >>> P.parse columnName "Users"

-- >>> P.parse columnName "id, name"

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
    <*> (P.space *> P.string "=" *> P.space *> parsedCell)


test_whereClause :: Test
test_whereClause =
  TestList
    [ 
      P.parse whereClause "Users = 1" ~?= Right (WhereClause {whereClauseColumn = ColumnName "Users", whereClauseValue = CellInt 1}),
      P.parse whereClause "id = 1" ~?= Right (WhereClause {whereClauseColumn = ColumnName "id", whereClauseValue = CellInt 1}),
      P.parse whereClause "id = 1 AND user = rias" ~?= Right (WhereClause {whereClauseColumn = ColumnName "id", whereClauseValue = CellInt 1})
    ]

parseColumnNames :: String -> Either P.ParseError [ColumnName]
parseColumnNames = P.parse (P.sepBy columnName (P.char ',' <* P.space))

test_parseColumnNames :: Test
test_parseColumnNames =
  TestList
    [ 
      parseColumnNames "id, name" ~?= Right [ColumnName "id", ColumnName "name"],
      parseColumnNames "id" ~?= Right [ColumnName "id"]
    ]

parseWhereClauses :: String -> Either P.ParseError [WhereClause]
parseWhereClauses = P.parse (P.string "WHERE" *> P.space *> whereClause `P.sepBy` (P.space *> P.string "AND" <* P.space))


test_parseWhereClauses :: Test
test_parseWhereClauses =
  TestList
    [ 
      parseWhereClauses "WHERE id = 1 AND user = \"rias\"" ~?= Right [WhereClause {whereClauseColumn = ColumnName "id", whereClauseValue = CellInt 1}, WhereClause {whereClauseColumn = ColumnName "user", whereClauseValue = CellString "rias"}],
      parseWhereClauses "WHERE id = 1" ~?= Right [WhereClause {whereClauseColumn = ColumnName "id", whereClauseValue = CellInt 1}]
    ]

selectParser :: Parser Statement
selectParser = StatementSelect 
    <$> (P.string "SELECT" *> P.space *> columnNames <* P.space)
    <*> (P.string "FROM" *> P.space *> parsedTableName)
    <*> optionalWhereClause
  where
    columnNames = P.sepBy columnName (P.char ',' <* P.space)
    optionalWhereClause = (P.space *> P.string "WHERE" *> P.space *> whereClauses) <|> pure []
    whereClauses = whereClause `P.sepBy` (P.space *> P.string "AND" <* P.space)


insertParser =
  liftA3
    (\tableName columnNames cells -> StatementInsert (Row $ Map.fromList (zip columnNames cells)) tableName)
    (P.string "INSERT INTO" *> P.space *> parsedTableName <* P.space)
    (P.char '(' *> P.sepBy1 columnName (P.char ',' *> P.space) <* P.string ")" <* P.space <* P.string "VALUES" <* P.space)
    (P.sepBy1 parsedCell (P.char ',' *> P.space)  <* P.eof)


dropParser :: Parser Statement
dropParser = StatementDrop <$> (P.string "DROP TABLE" *> P.space *> parsedTableName <* P.eof)

dropIndexParser :: Parser Statement
dropIndexParser = StatementDropIndex <$> (P.string "DROP INDEX" *> P.space *> indexName <* P.eof)
  where
    indexName = IndexName <$> identifier

createParser :: Parser Statement
createParser = StatementCreate <$> (P.string "CREATE TABLE" *> P.space *> parsedTableName) <*> (P.space *> P.char '(' *> P.sepBy columnDefinition (P.char ',' <* P.space) <* P.char ')' <* P.eof)
  where
    columnDefinition = ColumnDefinition <$> columnName <*> (P.space *> cellType)
    cellType = CellTypeInt <$ P.string "INT" <|> CellTypeString <$ P.string "STRING" <|> CellTypeBool <$ P.string "BOOL"

createIndexParser :: Parser Statement
createIndexParser = StatementCreateIndex <$> (P.string "CREATE INDEX" *> P.space *> indexName) <*> (P.space *> P.string "ON" *> P.space *> parsedTableName) <*> (P.space *> P.char '(' *> P.sepBy columnName (P.char ',' <* P.space) <* P.char ')' <* P.eof)
  where
    indexName = IndexName <$> identifier

parsedAlterAction :: Parser AlterAction
parsedAlterAction = P.choice [addColumn, dropColumn, renameColumn, modifyColumn]
  where
    addColumn = AddColumn <$> (P.string "ADD COLUMN" *> P.space *> columnDefinition)
    dropColumn = DropColumn <$> (P.string "DROP COLUMN" *> P.space *> columnName)
    renameColumn = RenameColumn <$> (P.string "RENAME COLUMN" *> P.space *> columnName) <*> (P.space *> P.string "TO" *> P.space *> columnName)
    columnDefinition = ColumnDefinition <$> columnName <*> (P.space *> cellType)
    cellType = CellTypeInt <$ P.string "INT" <|> CellTypeString <$ P.string "STRING" <|> CellTypeBool <$ P.string "BOOL"

alterParser :: Parser Statement
alterParser = StatementAlter <$> (P.string "ALTER TABLE" *> P.space *> parsedTableName) <*> (P.space *> parsedAlterAction <* P.eof)

-- Combine all statement parsers
statementParser :: Parser Statement
statementParser = P.choice [selectParser, insertParser, alterParser, dropParser, dropIndexParser, createParser, createIndexParser]

-- Example of how to use the parser
parseSQL :: String -> Either P.ParseError Statement
parseSQL = P.parse statementParser

testParseSQL :: Test
testParseSQL = TestList [test_selectQuery, test_insertQuery, test_alterQuery, test_dropQuery, test_dropIndexQuery, test_createTableQuery, test_createIndexQuery]

test_selectQuery :: Test
test_selectQuery = 
    TestList
    [ 
      parseSQL "SELECT id, name FROM users WHERE id = 1" ~?= Right (StatementSelect [ColumnName "id", ColumnName "name"] (TableName "users") [WhereClause (ColumnName "id") (CellInt 1)]), 
      parseSQL "SELECT id, name FROM users" ~?= Right (StatementSelect [ColumnName "id", ColumnName "name"] (TableName "users") []),
      parseSQL "SELECT id, name FROM users WHERE id = 1 AND name = \"rias\"" ~?= Right (StatementSelect [ColumnName "id", ColumnName "name"] (TableName "users") [WhereClause (ColumnName "id") (CellInt 1), WhereClause (ColumnName "name") (CellString "rias")]),
      parseSQL "SELECT id FROM users" ~?= Right (StatementSelect [ColumnName "id"] (TableName "users") []),
      parseSQL "select id FROM users" ~?= Left "No parses",
      parseSQL "SELECT id from users" ~?= Left "No parses"
    ]

test_insertQuery :: Test
test_insertQuery = 
    TestList
    [ 
      parseSQL "INSERT INTO users (num, name) VALUES 1, \"John Doe\"" ~?= Right (StatementInsert (Row $ Map.fromList [(ColumnName "num", CellInt 1), (ColumnName "name", CellString "John Doe")]) (TableName "users")),
      parseSQL "INSERT INTO users (num, name, isEligible) VALUES 1, \"John Doe\", TRUE" ~?= Right (StatementInsert (Row $ Map.fromList [(ColumnName "num", CellInt 1), (ColumnName "name", CellString "John Doe"), (ColumnName "isEligible", CellBool True)]) (TableName "users")),
      parseSQL "INSERT INTO users (num) VALUES 1" ~?= Right (StatementInsert (Row $ Map.fromList [(ColumnName "num", CellInt 1)]) (TableName "users")),
      parseSQL "INSERT INTO users VALUES" ~?= Left "No parses",
      parseSQL "INSERT INTO users" ~?= Left "No parses",
      parseSQL "INSERT INTO users (num, name, isEligible) VALUES 1, \"John Doe\" AND 1" ~?= Left "No parses"
    ]

test_alterQuery :: Test
test_alterQuery = 
    TestList
    [ 
      parseSQL "ALTER TABLE users ADD COLUMN id INT" ~?= Right (StatementAlter (TableName "users") (AddColumn (ColumnDefinition (ColumnName "id") CellTypeInt))),
      parseSQL "ALTER TABLE users DROP COLUMN id" ~?= Right (StatementAlter (TableName "users") (DropColumn (ColumnName "id"))),
      parseSQL "ALTER TABLE users RENAME COLUMN id TO newid" ~?= Right (StatementAlter (TableName "users") (RenameColumn (ColumnName "id") (ColumnName "newid"))),
      parseSQL "ALTER TABLE users ADD COLUMN id INT AND DROP id" ~?= Left "No parses",
      parseSQL "ALTER TABLE users" ~?= Left "No parses"
    ]

test_dropQuery :: Test
test_dropQuery = 
    TestList
    [ 
      parseSQL "DROP INDEX users" ~?= Right (StatementDropIndex (IndexName "users")),
      parseSQL "DROP INDEX users AND newusers" ~?= Left "No parses",
      parseSQL "DROP INDEX" ~?= Left "No parses"
    ]

test_dropIndexQuery :: Test
test_dropIndexQuery = 
    TestList
    [ 
      parseSQL "DROP TABLE users" ~?= Right (StatementDrop (TableName "users")),
      parseSQL "DROP TABLE users AND newusers" ~?= Left "No parses",
      parseSQL "DROP TABLE" ~?= Left "No parses"
    ]

test_createTableQuery :: Test
test_createTableQuery = 
    TestList
    [ 
      parseSQL "CREATE TABLE users (id INT, name STRING, isEligible BOOL)" ~?= Right (StatementCreate (TableName "users") [ColumnDefinition (ColumnName "id") CellTypeInt, ColumnDefinition (ColumnName "name") CellTypeString, ColumnDefinition (ColumnName "isEligible") CellTypeBool]),
      parseSQL "CREATE TABLE users (id INT, name STRING, isEligible BOOL) AND newusers (id INT, name STRING, isEligible BOOL)" ~?= Left "No parses",
      parseSQL "CREATE TABLE users" ~?= Left "No parses",
      parseSQL "CREATE TABLE users (id int, name string, isEligible bool)" ~?= Left "No parses"
    ]

test_createIndexQuery :: Test
test_createIndexQuery = 
    TestList [
        parseSQL "CREATE INDEX index ON users (id, name)" ~?= Right (StatementCreateIndex (IndexName "index") (TableName "users") [ColumnName "id", ColumnName "name"]),
        parseSQL "CREATE INDEX index ON users (id, name) AND newusers (id, name)" ~?= Left "No parses",
        parseSQL "CREATE INDEX index ON users" ~?= Left "No parses",
        parseSQL "CREATE INDEX index ON users (id)" ~?= Right (StatementCreateIndex (IndexName "index") (TableName "users") [ColumnName "id"])
    ]

-- Function to run all tests
runTests :: IO Counts
runTests = runTestTT testParseSQL