module Execute where

import Control.Concurrent.STM
import Types

emptyDB :: DBRef
emptyDB = undefined

execStatement :: DBRef -> Statement -> DBRef -> STM Response
execStatement = undefined