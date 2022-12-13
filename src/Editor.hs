module Editor 
    ( editorApp
    , drawGrid
    ) where

import GridMap (Grid(..), Tile(..), getTile, gridToGraph, adjacents, simpleGrid, getNeighbors, setNeighbors, gridFromList)

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Table
import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S

drawInfo :: Widget ()
drawInfo = withBorderStyle BS.unicodeBold
  $ C.hCenter
  $ hLimit 80
  $ vLimit 400
  $ B.borderWithLabel (str "First hit enter. Then type which player you are.")
  $ vBox $ map (uncurry drawKey)
  $ [ ("h", "Human Player (YOU!)")
    , ("u", "Up Bot (Always moves up)")
    , ("r", "Random Bot")
    , ("m", "monte carlo bot")
    ]
    where
      drawKey act key = (padRight Max $ padLeft (Pad 1) $ str act)
                        <+> (padLeft Max $ padRight (Pad 1) $ str key)


data EditState = EditState
  { grid :: Grid
  , cursor :: (Int, Int)
  }

testTable :: Table ()
testTable =
    surroundingBorder False $
    table [ [str "test", str "table"]
          , [str "is",    str "here"]
          ]

drawTable :: Table () -> Widget ()
drawTable table = C.center $ renderTable table

drawTile :: Tile -> String
drawTile tile = if traversible tile then show (elevation tile) else "X"

gridTable :: Grid -> Table ()
gridTable grid = table (map (\r -> map (\c -> str (drawTile (getTile grid r c))) [0..cols grid - 1]) [0..rows grid - 1])

drawGrid :: Grid -> Widget ()
drawGrid grid = C.center $ renderTable (gridTable grid)

editorApp :: B.App EditState e n
editorApp = undefined
--     B.App
--   { B.appDraw = drawInfo
--   , B.appChooseCursor = (\_ _ -> Nothing)
--   , B.appHandleEvent = handleEvent
--   , B.appStartEvent = startEvent
--   , B.appAttrMap = attrMap
--   }

-- handleEvent :: Grid -> B.BrickEvent n e -> B.EventM n (B.Next Grid)
-- handleEvent grid (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt grid
-- handleEvent grid (B.VtyEvent (V.EvKey (V.KChar 'r') [])) = B.continue grid

