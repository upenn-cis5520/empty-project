module Main where

-- Make sure you run this via `stack run` in the top-level folder.
-- If you run it from `src` or `test` the tests won't be able to 
-- locate the files.

import qualified LuStepper as LS
import qualified LuParser  as LP

main :: IO ()
main = do 
  putStrLn "*** Testing LuStepper ***"
  LS.test_all -- unit tests 
  LS.qc       -- quickcheck properties
  putStrLn "*** Testing LuParser ***"
  LP.test_all -- unit tests
  LP.qc       -- quickcheck properties







