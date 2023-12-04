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

newtype TableName = TableName {unTableName :: String}

data CellType = CellTypeInt | CellTypeString | CellTypeBool

data Cell where
  CellInt :: Int -> Cell
  CellString :: String -> Cell
  CellBool :: Bool -> Cell

newtype Row = TVar [Cell]

newtype ColumnName = ColumnName String

newtype IndexName = IndexName String

data WhereClause = WhereClause
  { whereClauseColumn :: ColumnName,
    whereClauseValue :: Cell
  }

data ColumnDefinition = ColumnDefinition
  { columnDefinitionName :: ColumnName,
    columnDefinitionType :: CellType
  }

data Statement
  = StatementSelect [ColumnName] TableName [WhereClause]
  | StatementInsert Row TableName
  | StatementCreate TableName [ColumnDefinition]
  | StatementCreateIndex IndexName TableName [ColumnName]
  | StatementDrop TableName
  | StatementDropIndex IndexName

newtype Transaction = Atomic [Statement]

newtype PrimaryKey = PrimaryKey {unPrimaryKey :: Int}

data Table = Table
  { tableName :: TableName,
    tableDefinition :: [ColumnDefinition],
    tableRows :: Map PrimaryKey Row,
    tableNextPrimaryKey :: PrimaryKey,
    tableIndices :: [IndexName]
  }

data Index = Index
  { indexName :: IndexName,
    indexTable :: TableName,
    indexColumns :: [ColumnName],
    indexData :: Map [Cell] [Row]
  }

data Database = Database
  { databaseTables :: Map TableName (TVar Table),
    databaseIndices :: Map IndexName (TVar Index)
  }

newtype Response = Response {res :: String} -- TODO: better response types

type DBRef = TVar Database
