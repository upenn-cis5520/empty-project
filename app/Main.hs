module Main where

import Execute
import StatementParser

main :: IO ()
main = do
  putStrLn "Welcome to the Haskell Database Engine!"
  --   db <- emptyDB

  putStr ("HaskellDB" ++ "> ")
  str <- getLine
  case parseSQL str of
    Left err -> do
      putStrLn err
    Right statement -> do
      print statement

--   db' <- execStatement db statement

--   repl db'

--   repl emptyDB
