module Main where

import Graph
import GridMap (Place, Tile, Grid, getTile, edge, gridToGraph, adjacents, simpleGrid, getNeighbors, setNeighbors, drawGrid)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor, attrName, simpleMain
  , halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
main :: IO ()
main = do
    simpleMain (drawGrid (simpleGrid 3 3))
