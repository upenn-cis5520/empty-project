{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor 
    ( 
      GridInfo(..)
    , editorApp
    , drawGrid
    , initEditor
    ) where

import GridMap ( Grid(..), Tile(..)
               , getTile, gridToGraph, adjacents, simpleGrid, getNeighbors
               , setNeighbors, setTraversible, setElevation, setTile
               , gridFromList)

import Control.Monad (forever, void)
import Lens.Micro ((^.))
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
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )  
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Brick.Widgets.Edit as E

data Name = ElevationField
          | TraversibleField
          | GridField
          | CurRowField
          | CurColField
          deriving (Eq, Ord, Show)
blockedAttr = attrName "blocked"
lowAttr = attrName "low"
mediumAttr = attrName "medium"
highAttr = attrName "high"
selectedAttr = attrName "selected"


data GridInfo = GridInfo
  { _elevationVal :: Float
  , _isTraversible :: Bool
  , _curRow :: Int
  , _curCol :: Int
  , _grid :: Grid
  }

-- data EditState = EditState
--   { 
--     grid :: Grid
--   , form :: Form TileInfo () Name
--   }


makeLenses ''GridInfo

mkForm :: GridInfo -> Form GridInfo () Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [  label "Elevation" @@=
                   editShowableField elevationVal ElevationField
               , label "" @@=
                   checkboxField isTraversible TraversibleField "Traversible?"
               , label "Current Row" @@=
                   editShowableField curRow CurRowField
                , label "Current Column" @@=
                   editShowableField curCol CurColField
               
               ]

drawTable :: Table Name -> Widget Name
drawTable table = C.center $ renderTable table

drawTile :: Tile -> Widget Name
drawTile t = case (traversible t, elevation t) of
    (True, elev) -> withAttr heightAttr $ (str . show) elev
    (False, _) -> withAttr blockedAttr $ str "X"
    where
        heightAttr = case elevation t of
            elev | elev < -5 -> lowAttr
                 | elev < 5 -> mediumAttr
                 | otherwise -> highAttr

gridTable :: Grid -> Table Name
gridTable grid = table (map (\r -> map (\c -> drawTile (getTile grid r c)) [0..cols grid - 1]) [0..rows grid - 1])

drawGrid :: Grid -> Widget Name
drawGrid grid = C.center $ renderTable (gridTable grid)


-- drawEditor :: EditState -> [Widget Name]
-- drawEditor s = [drawGrid (grid s) <=> drawForm (form s) ]
--     where 
--         drawForm f = C.vCenter $ C.hCenter $ renderForm f

draw :: Form GridInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter gridVisual]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        gridVisual = B.border $ padTop (Pad 1) $ drawGrid (_grid (formState f))

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , (blockedAttr, V.white `on` V.black)
  , (lowAttr, V.white `on` V.red)
  , (mediumAttr, V.white `on` V.yellow)
  , (highAttr, V.white `on` V.green)
  ]

editorApp :: B.App (Form GridInfo e Name) e Name
editorApp = B.App
  { B.appDraw = draw
  , B.appChooseCursor = focusRingCursor formFocus
  , B.appHandleEvent = handleEvent
  , B.appStartEvent = return ()
  , B.appAttrMap = const theMap
  }

-- | Move the cursor by the given amount, while keeping it inside of bounds
-- moveCursor :: (Int, Int) -> EventM Name EditState ()
-- moveCursor (r, c) = do
--     s <- B.get
--     let (r', c') = cursor s
--     let newCursor = (r' + r, c' + c)
--     let (r'', c'') = newCursor
--     let (rows', cols') = (rows (grid s), cols (grid s))
--     if r'' >= 0 && r'' < rows' && c'' >= 0 && c'' < cols'
--         then B.put s {cursor = newCursor}
--         else return ()


handleEvent :: B.BrickEvent Name e -> EventM Name (Form GridInfo e Name) ()
handleEvent ev = do
    s <- B.get
    f <- B.gets formFocus
    case ev of
      VtyEvent (V.EvResize {}) -> return ()
      VtyEvent (V.EvKey V.KEsc []) -> halt
      VtyEvent (V.EvKey V.KEnter []) -> updateGrid
      _ -> do
          handleFormEvent ev
          st <- gets formState
          modify $ setFieldValid (st^.curRow >= 0 && st^.curRow < rows (st^.grid)) CurRowField
          modify $ setFieldValid (st^.curCol >= 0 && st^.curCol < cols (st^.grid)) CurColField
      where
          updateGrid = do
              s <- B.get
              if allFieldsValid s
                  then do
                    let f = formState s
                    let grid = _grid f
                    let r = _curRow f
                    let c = _curCol f
                    let tile = getTile grid r c
                    let newTile = tile {elevation = _elevationVal f, traversible = _isTraversible f}
                    let newGrid = setTile grid r c newTile
                    B.put s {formState = f {_grid = newGrid}}
                    return ()
                  else return ()




  




initEditor :: Grid -> IO ()
initEditor grid = do
    chan <- newBChan 10
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    let app = editorApp
    let state = mkForm (GridInfo 0 True 0 0 grid)
    void $ customMain initialVty buildVty (Just chan) app state