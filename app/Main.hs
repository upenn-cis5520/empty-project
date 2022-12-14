{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GridMap 
    ( Grid(..)
    , simpleGrid    
    )   
import Editor 
    ( EditState(..)
    , TileInfo(..)
    , editorApp
    , drawGrid
    , initEditor
    )

import qualified Data.Text as T
import Lens.Micro ((^.))
import Lens.Micro.TH
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import qualified Graphics.Vty as V
import Brick
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
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

data Name = RowsField
          | ColumnsField
          deriving (Eq, Ord, Show)


data GridInfo =
    GridInfo { _rowCount      :: Int
             , _columnCount       :: Int
             }
             deriving (Show)

makeLenses ''GridInfo

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: GridInfo -> Form GridInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [
                 label "Rows" @@=
                   editShowableField rowCount RowsField
               , label "Columns" @@=
                   editShowableField columnCount ColumnsField
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form GridInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Enter number of rows and columns\n" <>
                     "- Must be an integers\n" <>
                     "- Press Enter to continue"
app :: App (Form GridInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \ev -> do
            f <- gets formFocus
            case ev of
                VtyEvent (V.EvResize {}) -> return ()
                VtyEvent (V.EvKey V.KEsc []) -> halt
                VtyEvent (V.EvKey V.KEnter []) -> halt
                _ -> do
                    handleFormEvent ev

                    -- Example of external validation:
                    -- Require age field to contain a value that is at least 18.
                    st <- gets formState
                    modify $ setFieldValid (st^.rowCount > 0) RowsField
                    modify $ setFieldValid (st^.columnCount > 0) ColumnsField

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialGridInfo = GridInfo { _rowCount = 5
                                   , _columnCount = 5
                                   }
        f = mkForm initialGridInfo

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f

    putStrLn "The starting form state was:"
    print initialGridInfo

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then let initialGrid = simpleGrid (_rowCount (formState f')) (_columnCount (formState f'))
            in initEditor initialGrid
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
    
    
