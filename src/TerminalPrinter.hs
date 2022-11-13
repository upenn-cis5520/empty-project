{-
This module contains all functions related to printing ascii videos
from text file to the terminal, in a specified rate.
-}

module TerminalPrinter where

import Brick
  ( App (..),
    AttrMap,
    BrickEvent (..),
    EventM,
    Next,
    Widget,
    neverShowCursor,
  )
import System.FilePath ()
import Variables qualified

{-
Type for Application State (s)
-}
data VidState = VidState
  { _path :: FilePath,
    _idx :: Int,
    _fps :: Int,
    _width :: Int,
    _height :: Int,
    _content :: String,
    _finished :: Bool
  }
  deriving (Show)

{-
Type for Event (e)
-}
data Tick = Tick

{-
Type for Resource Name (n)
-}
type ResourceName = ()

{-
State transfer function from one frame to the next frame
-}
nextFrame :: VidState -> VidState
nextFrame = undefined

{-
State transfer function that finishes playing
-}
finish :: VidState -> VidState
finish = undefined

handleEvent :: VidState -> BrickEvent ResourceName Tick -> EventM ResourceName (Next VidState)
handleEvent = undefined

drawUI :: VidState -> [Widget ResourceName]
drawUI = undefined

defaultMap :: AttrMap
defaultMap = undefined

app :: App VidState Tick ResourceName
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const defaultMap
    }

main :: IO ()
main = undefined