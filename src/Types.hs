module Types where

-- import ClassyPrelude hiding (Index)
-- import Control.Lens.TH (makeLenses, makePrisms)

-- import Data.Aeson
--   ( FromJSON,
--     FromJSONKey,
--     ToJSON,
--     ToJSONKey,
--     Value (Bool, Number, String),
--     object,
--     parseJSON,
--     toJSON,
--     withObject,
--     (.:),
--     (.=),
--   )

import Control.Concurrent.STM
import Control.Monad (fail)
import Control.Monad.State
import Data.Map (Map)

-- import Data.Aeson

newtype TableName = TableName {unTableName :: String} deriving (Eq, Show)

data CellType = CellTypeInt | CellTypeString | CellTypeBool deriving (Eq, Show)

data Cell where
  CellInt :: Int -> Cell
  CellString :: String -> Cell
  CellBool :: Bool -> Cell
  deriving (Eq, Show)

newtype Row = TVar [Cell] deriving (Eq, Show)

newtype ColumnName = ColumnName String deriving (Eq, Show)

newtype IndexName = IndexName String deriving (Eq, Show)

data WhereClause = WhereClause
  { whereClauseColumn :: ColumnName,
    whereClauseValue :: Cell
  }
  deriving (Eq, Show)

data ColumnDefinition = ColumnDefinition
  { columnDefinitionName :: ColumnName,
    columnDefinitionType :: CellType
  }
  deriving (Eq, Show)

data AlterAction
  = AddColumn ColumnDefinition
  | DropColumn ColumnName
  | RenameColumn ColumnName ColumnName
  | ModifyColumn ColumnName ColumnDefinition
  deriving (Eq, Show)

data Statement
  = StatementSelect [ColumnName] TableName [WhereClause]
  | StatementInsert Row TableName
  | StatementAlter TableName AlterAction
  | StatementCreate TableName [ColumnDefinition]
  | StatementCreateIndex IndexName TableName [ColumnName]
  | StatementDrop TableName
  | StatementDropIndex IndexName
  deriving (Eq, Show)

newtype Transaction = Atomic [Statement] deriving (Eq, Show)

newtype PrimaryKey = PrimaryKey {unPrimaryKey :: Int} deriving (Eq, Show)

data Table = Table
  { tableName :: TableName,
    tableDefinition :: [ColumnDefinition],
    tableRows :: Map PrimaryKey Row,
    tableNextPrimaryKey :: PrimaryKey,
    tableIndices :: [IndexName]
  }
  deriving (Eq, Show)

data Index = Index
  { indexName :: IndexName,
    indexTable :: TableName,
    indexColumns :: [ColumnName],
    indexData :: Map [Cell] [Row]
  }
  deriving (Eq, Show)

data Database = Database
  { databaseTables :: Map TableName (TVar Table),
    databaseIndices :: Map IndexName (TVar Index)
  }
  deriving (Eq)

newtype Response = Response {res :: String} deriving (Eq, Show)

type DBRef = TVar Database
