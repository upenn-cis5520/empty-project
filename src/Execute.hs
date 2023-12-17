module Execute where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IntMap (update)
import Data.Map qualified as Map
import Types

emptyDB :: STM DBRef
emptyDB = newTVar $ Database mempty mempty

defaultCellValue :: CellType -> Cell
defaultCellValue CellTypeInt = CellInt 0
defaultCellValue CellTypeString = CellString ""
defaultCellValue CellTypeBool = CellBool False

executeStatement :: (MonadDatabase m) => Statement -> m (Either StatementFailureType [Row])
executeStatement statement = case statement of
  StatementSelect cols table whereClauses -> executeSelect cols table whereClauses
  StatementInsert row table -> executeInsert row table
  StatementAlter table action -> executeAlter table action
  StatementCreate table colDefs -> executeCreate table colDefs
  StatementCreateIndex indexName table cols -> executeCreateIndex indexName table cols
  StatementDrop table -> executeDrop table
  StatementDropIndex indexName -> executeDropIndex indexName

executeSelect :: (MonadDatabase m) => [ColumnName] -> TableName -> [WhereClause] -> m (Either StatementFailureType [Row])
executeSelect cols tableName whereClauses = do
  db <- get
  case Map.lookup tableName (databaseTables db) of
    Nothing -> return $ Left TableDoesNotExist
    Just tableVar -> do
      table <- liftIO $ readTVarIO tableVar
      let filteredRows = filter (applyWhereClauses whereClauses) (Map.elems $ tableRows table)
      return $ Right $ map (selectColumns cols) filteredRows

applyWhereClauses :: [WhereClause] -> Row -> Bool
applyWhereClauses clauses (Row cellsMap) =
  all (\(WhereClause col val) -> maybe False (cellMatchesValue val) (Map.lookup col cellsMap)) clauses

cellMatchesValue :: Cell -> Cell -> Bool
cellMatchesValue cell value = case (cell, value) of
  (CellInt a, CellInt b) -> a == b
  (CellString a, CellString b) -> a == b
  (CellBool a, CellBool b) -> a == b
  _ -> False

selectColumns :: [ColumnName] -> Row -> Row
selectColumns cols (Row cellsMap) =
  Row $ Map.filterWithKey (\k _ -> k `elem` cols) cellsMap

executeInsert :: (MonadDatabase m) => Row -> TableName -> m (Either StatementFailureType [Row])
executeInsert row tableName = do
  db <- get
  case Map.lookup tableName (databaseTables db) of
    Nothing -> return $ Left TableDoesNotExist
    Just tableVar -> do
      table <- liftIO $ readTVarIO tableVar
      let newKey = tableNextPrimaryKey table
      let updatedRows = Map.insert newKey row (tableRows table)
      let updatedTable = table {tableRows = updatedRows, tableNextPrimaryKey = incrementPrimaryKey newKey}
      liftIO $ atomically $ writeTVar tableVar updatedTable
      return $ Right []
  where
    incrementPrimaryKey :: PrimaryKey -> PrimaryKey
    incrementPrimaryKey (PrimaryKey k) = PrimaryKey (k + 1)

executeAlter :: (MonadDatabase m) => TableName -> AlterAction -> m (Either StatementFailureType [Row])
executeAlter tableName action = do
  db <- get
  case Map.lookup tableName (databaseTables db) of
    Nothing -> return $ Left TableDoesNotExist
    Just tableVar -> do
      table <- liftIO $ readTVarIO tableVar
      newTable <- liftIO $ applyAlterAction table action
      liftIO $ atomically $ writeTVar tableVar newTable
      return $ Right []

applyAlterAction :: Table -> AlterAction -> IO Table
applyAlterAction table action = case action of
  AddColumn colDef -> return $ addColumn table colDef
  DropColumn colName -> return $ dropColumn table colName
  RenameColumn oldName newName -> return $ renameColumn table oldName newName

addColumn :: Table -> ColumnDefinition -> Table
addColumn table colDef =
  table
    { tableDefinition = tableDefinition table ++ [colDef],
      tableRows = Map.map (addCellToRow colDef) (tableRows table)
    }

dropColumn :: Table -> ColumnName -> Table
dropColumn table colName =
  table
    { tableDefinition = filter ((/= colName) . columnDefinitionName) (tableDefinition table),
      tableRows = Map.map (removeCellFromRow colName) (tableRows table)
    }

renameColumn :: Table -> ColumnName -> ColumnName -> Table
renameColumn table oldName newName =
  table
    { tableDefinition = map (updateColDefName oldName newName) (tableDefinition table),
      tableRows = Map.map (renameCellInRow oldName newName) (tableRows table)
    }
  where
    updateColDefName :: ColumnName -> ColumnName -> ColumnDefinition -> ColumnDefinition
    updateColDefName oldName newName colDef
      | columnDefinitionName colDef == oldName = colDef {columnDefinitionName = newName}
      | otherwise = colDef

-- Need to update all column changes in the Row type as well:
addCellToRow :: ColumnDefinition -> Row -> Row
addCellToRow (ColumnDefinition colName colType) (Row rowMap) =
  let defaultCell = defaultCellValue colType
   in Row $
        Map.insert
          colName
          defaultCell
          rowMap

removeCellFromRow :: ColumnName -> Row -> Row
removeCellFromRow colName (Row rowMap) =
  Row $ Map.delete colName rowMap

renameCellInRow :: ColumnName -> ColumnName -> Row -> Row
renameCellInRow oldName newName (Row rowMap) =
  case Map.lookup oldName rowMap of
    Just cell -> Row $ Map.insert newName cell $ Map.delete oldName rowMap
    Nothing -> Row rowMap

executeCreate :: (MonadDatabase m) => TableName -> [ColumnDefinition] -> m (Either StatementFailureType [Row])
executeCreate tableName colDefs = do
  db <- get
  case Map.lookup tableName (databaseTables db) of
    Just _ -> return $ Left TableAlreadyExists
    Nothing -> do
      let newTable =
            Table
              { tableName = tableName,
                tableDefinition = colDefs,
                tableRows = Map.empty,
                tableNextPrimaryKey = PrimaryKey 0,
                tableIndices = []
              }
      newTableVar <- liftIO $ newTVarIO newTable
      let updatedTables = Map.insert tableName newTableVar (databaseTables db)
      let updatedDb = db {databaseTables = updatedTables}
      put updatedDb
      return $ Right []

executeCreateIndex :: (MonadDatabase m) => IndexName -> TableName -> [ColumnName] -> m (Either StatementFailureType [Row])
executeCreateIndex indexName tableName colNames = do
  db <- get
  case Map.lookup tableName (databaseTables db) of
    Nothing -> return $ Left TableDoesNotExist
    Just tableVar -> do
      table <- liftIO $ readTVarIO tableVar
      case Map.lookup indexName (databaseIndices db) of
        Just _ -> return $ Left IndexAlreadyExists
        Nothing -> do
          let newIndex = Index {indexName = indexName, indexTable = tableName, indexColumns = colNames, indexData = Map.empty}
          newIndexVar <- liftIO $ newTVarIO newIndex
          let updatedIndices = Map.insert indexName newIndexVar (databaseIndices db)
          let updatedDb = db {databaseIndices = updatedIndices}
          put updatedDb
          return $ Right []

executeDrop :: (MonadDatabase m) => TableName -> m (Either StatementFailureType [Row])
executeDrop tableName = do
  db <- get
  case Map.lookup tableName (databaseTables db) of
    Nothing -> return $ Left TableDoesNotExist
    Just _ -> do
      let updatedTables = Map.delete tableName (databaseTables db)
      let updatedDb = db {databaseTables = updatedTables}
      put updatedDb
      return $ Right []

executeDropIndex :: (MonadDatabase m) => IndexName -> m (Either StatementFailureType [Row])
executeDropIndex indexName = do
  db <- get
  case Map.lookup indexName (databaseIndices db) of
    Nothing -> return $ Left IndexDoesNotExist
    Just _ -> do
      let updatedIndices = Map.delete indexName (databaseIndices db)
      let updatedDb = db {databaseIndices = updatedIndices}
      put updatedDb
      return $ Right []
