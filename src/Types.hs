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
import RedBlackGADT3

-- import Data.Aeson

newtype TableName = TableName {unTableName :: String}

data AtomType = AtomTypeInt | AtomTypeString | AtomTypeBool

data Atom where
  AtomInt :: Int -> Atom
  AtomString :: String -> Atom
  AtomBool :: Bool -> Atom

type Row = TVar [Atom]

newtype ColumnName = ColumnName String

newtype IndexName = IndexName String

data WhereClause = WhereClause
  { whereClauseColumn :: ColumnName,
    whereClauseValue :: Atom
  }

data ColumnDefinition = ColumnDefinition
  { columnDefinitionName :: ColumnName,
    columnDefinitionType :: AtomType
  }

data Statement
  = StatementSelect [ColumnName] TableName [WhereClause]
  | StatementInsert Row TableName
  | StatementCreate TableName [ColumnDefinition]
  | StatementCreateIndex IndexName TableName [ColumnName]
  | StatementDrop TableName
  | StatementDropIndex IndexName

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
    indexData :: Map [Atom] [Row]
  }

data Database = Database
  { databaseTables :: Map TableName Table,
    databaseIndices :: Map IndexName Index
  }

newtype Response = Response {res :: String} -- TODO: better response types

type MonadDB = State Database Response
