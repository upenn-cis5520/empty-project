{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor 
    ( EditState(..)
    , TileInfo(..)
    , editorApp
    , drawGrid
    , initEditor
    ) where

import GridMap (Grid(..), Tile(..), getTile, gridToGraph, adjacents, simpleGrid, getNeighbors, setNeighbors, gridFromList)

import Control.Monad (forever, void)
import Lens.Micro.TH
import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Table
import qualified Brick as B
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S

data Name = ElevationField
          | TraversibleField
          deriving (Eq, Ord, Show)



data TileInfo = TileInfo
  { _elevationVal :: Int
  , _isTraversible :: Bool
  }

data EditState = EditState
  { grid :: Grid
  , cursor :: (Int, Int)
  , form :: Form TileInfo () Name
  }

-- | Move the cursor up, but not outside the grid.
-- moveCursorUp :: EditState -> EventM Name EditState ()
-- moveCursorUp s = do
--     let (r, c) = cursor s
--     if r > 0
--         then continue s { cursor = (r - 1, c) }
--         else continue s

-- left :: EditState -> EditState

makeLenses ''TileInfo

mkForm :: TileInfo -> Form TileInfo () Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [  label "Elevation" @@=
                   editShowableField elevationVal ElevationField
               , label "" @@=
                   checkboxField isTraversible TraversibleField "Traversible?"
               ]

drawTable :: Table Name -> Widget Name
drawTable table = C.center $ renderTable table

drawTile :: Tile -> String
drawTile tile = if traversible tile then show (elevation tile) else "X"

gridTable :: Grid -> Table Name
gridTable grid = table (map (\r -> map (\c -> str (drawTile (getTile grid r c))) [0..cols grid - 1]) [0..rows grid - 1])

drawGrid :: Grid -> Widget Name
drawGrid grid = C.center $ renderTable (gridTable grid)

-- draw :: Form UserInfo e () -> [Widget ()]
-- draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
--     where
--         form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
--         help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
--         body = str $ "- Name is free-form text\n" <>
--                      "- Age must be an integer (try entering an\n" <>
--                      "  invalid age!)\n" <>
--                      "- Handedness selects from a list of options\n" <>
--                      "- The last option is a checkbox\n" <>
--                      "- Enter/Esc quit, mouse interacts with fields"
drawEditor :: EditState -> [Widget Name]
drawEditor s = [drawGrid (grid s) <=> drawForm (form s) <=> drawCursor (cursor s)]
    where 
        drawForm f = C.vCenter $ C.hCenter $ renderForm f
        drawCursor (r, c) = C.vCenter $ C.hCenter $ str $ "Cursor: " ++ show r ++ ", " ++ show c  
editorApp :: B.App EditState e Name
editorApp = B.App
  { B.appDraw = drawEditor
  , B.appChooseCursor = B.showFirstCursor
  , B.appHandleEvent = handleEvent
  , B.appStartEvent = return ()
  , B.appAttrMap = const $ attrMap V.defAttr []
  }

handleEvent :: B.BrickEvent Name e -> EventM Name EditState ()
handleEvent (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt
handleEvent (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt
-- handleEvent (B.VtyEvent (V.EvKey (V.KUp) [])) = moveCursorUp


initEditor :: Grid -> IO ()
initEditor grid = do
    chan <- newBChan 10
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    let app = editorApp
    let state = EditState grid (0, 0) (mkForm (TileInfo 0 True))
    void $ customMain initialVty buildVty (Just chan) app state