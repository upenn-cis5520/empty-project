module DFDB.Types where

-- import ClassyPrelude hiding (Index)
-- import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad (fail)
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

import Data.Map (Map)
import Data.Text (Text)

-- import Data.Aeson

newtype TableName = TableName {unTableName :: Text}

data AtomType = AtomTypeInt | AtomTypeString | AtomTypeBool

data Atom where
  AtomInt :: Int -> Atom
  AtomString :: Text -> Atom
  AtomBool :: Bool -> Atom

type Row = [Atom]

newtype ColumnName = ColumnName Text

newtype IndexName = IndexName Text

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

data ColumnDefinition = ColumnDefinition
  { columnDefinitionName :: ColumnName,
    columnDefinitionType :: AtomType
  }

data Table = Table
  { tableName :: TableName,
    tableDefinition :: [ColumnDefinition],
    tableRows :: TreeMap PrimaryKey Row,
    tableNextPrimaryKey :: PrimaryKey,
    tableIndices :: [IndexName]
  }

data Index = Index
  { indexName :: IndexName,
    indexTable :: TableName,
    indexColumns :: [ColumnName],
    indexData :: TreeMap [Atom] [Row]
  }

data Database = Database
  { databaseTables :: Map TableName Table,
    databaseIndices :: Map IndexName Index
  }