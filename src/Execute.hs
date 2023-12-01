module Execute where

import Types

emptyDB :: MonadDB
emptyDB = undefined

execStatement :: MonadDB -> Statement -> MonadDB
execStatement = undefined