module Execute where

import Types

empty :: MonadDB
empty = undefined

execStatement :: MonadDB -> Statement -> MonadDB
execStatement = undefined