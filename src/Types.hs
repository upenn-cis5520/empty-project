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
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Map (Map)

-- import Data.Aeson

newtype TableName = TableName {unTableName :: String} deriving (Eq, Show, Ord)

data CellType = CellTypeInt | CellTypeString | CellTypeBool deriving (Eq, Show, Ord)

data Cell where
  CellInt :: Int -> Cell
  CellString :: String -> Cell
  CellBool :: Bool -> Cell
  deriving (Eq, Show, Ord)

-- newtype Row = TVar [Cell] deriving (Eq, Show, Ord)

newtype Row = Row (Map ColumnName Cell) deriving (Eq, Show, Ord)

newtype ColumnName = ColumnName String deriving (Eq, Show, Ord)

newtype IndexName = IndexName String deriving (Eq, Show, Ord)

data WhereClause = WhereClause
  { whereClauseColumn :: ColumnName,
    whereClauseValue :: Cell
  }
  deriving (Eq, Show, Ord)

data ColumnDefinition = ColumnDefinition
  { columnDefinitionName :: ColumnName,
    columnDefinitionType :: CellType
  }
  deriving (Eq, Show, Ord)

data StatementFailureType
  = TableDoesNotExist
  | ColumnDoesNotExist
  | IndexDoesNotExist
  | RowDoesNotExist
  | ColumnTypeMismatch
  | ColumnAlreadyExists
  | IndexAlreadyExists
  | TableAlreadyExists
  deriving (Eq, Show, Ord)

data AlterAction
  = AddColumn ColumnDefinition
  | DropColumn ColumnName
  | RenameColumn ColumnName ColumnName
  deriving (Eq, Show, Ord)

data Statement
  = StatementSelect [ColumnName] TableName [WhereClause]
  | StatementInsert Row TableName
  | StatementAlter TableName AlterAction
  | StatementCreate TableName [ColumnDefinition]
  | StatementCreateIndex IndexName TableName [ColumnName]
  | StatementDrop TableName
  | StatementDropIndex IndexName
  deriving (Eq, Show, Ord)

newtype Transaction = Atomic [Statement] deriving (Eq, Show, Ord)

newtype PrimaryKey = PrimaryKey {unPrimaryKey :: Int} deriving (Eq, Show, Ord)

data Table = Table
  { tableName :: TableName,
    tableDefinition :: [ColumnDefinition],
    tableRows :: Map PrimaryKey Row,
    tableNextPrimaryKey :: PrimaryKey,
    tableIndices :: [IndexName]
  }
  deriving (Eq, Show, Ord)

data Index = Index
  { indexName :: IndexName,
    indexTable :: TableName,
    indexColumns :: [ColumnName],
    indexData :: Map [Cell] [Row]
  }
  deriving (Eq, Show, Ord)

data Database = Database
  { databaseTables :: Map TableName (TVar Table),
    databaseIndices :: Map IndexName (TVar Index)
  }
  deriving (Eq)

-- type MonadDatabase m = (MonadState Database m)

class (MonadState Database m, MonadIO m) => MonadDatabase m

newtype Response = Response {res :: String} deriving (Eq, Show, Ord)

type DBRef = TVar Database
