module LuStepper where

import LuSyntax
import LuParser qualified
import LuEvaluator
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List qualified as List
import State (State)
import State qualified as S
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

step :: Block -> State Store Block
step (Block ((If e (Block ss1) (Block ss2)) : otherSs)) = do
  v <- evalE e
  if toBool v
    then return $ Block (ss1 ++ otherSs)
    else return $ Block (ss2 ++ otherSs)
step (Block (w@(While e (Block ss)) : otherSs)) = do
  v <- evalE e
  if toBool v
    then return $ Block (ss ++ [w] ++ otherSs)
    else return $ Block otherSs
step (Block (a@(Assign v e) : otherSs)) = do
  newState <- evalS a
  return $ Block otherSs
step (Block ((Repeat b e) : otherSs)) = step (Block (While (Op1 Not e) b : otherSs))
step (Block (empty : otherSs)) = return $ Block otherSs
step b@(Block []) = return b

-- | Make sure that we can step every block in every store
prop_step_total :: Block -> Store -> Bool
prop_step_total b s = case S.runState (step b) s of
  (b', s') -> True

-- | Evaluate this block for a specified number of steps
boundedStep :: Int -> Block -> State Store Block
boundedStep 0 b = return b
boundedStep _ b | final b = return b
boundedStep n b = step b >>= boundedStep (n - 1)

-- | Evaluate this block for a specified number of steps, using the specified store
steps :: Int -> Block -> Store -> (Block, Store)
steps n block = S.runState (boundedStep n block)

-- | Is this block completely evaluated?
final :: Block -> Bool
final (Block []) = True
final _ = False

allStep :: Block -> State Store Block
allStep b | final b = return b
allStep b = step b >>= allStep

-- | Evaluate this block to completion
execStep :: Block -> Store -> Store
execStep b = S.execState (allStep b)

prop_stepExec :: Block -> QC.Property
prop_stepExec b =
  not (final b) QC.==> final b1 QC.==> m1 == m2
  where
    (b1, m1) = S.runState (boundedStep 10 b) initialStore
    m2 = exec b initialStore

-- >>> runTestTT test_execStep

-- | test.lu:  arithemetic and while loops
tExecStepTest :: Test
tExecStepTest =
  "execStep wTest" ~:
    execStep wTest initialStore
      ~?= Map.fromList
        [ ( globalTableName,
            Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 10)]
          )
        ]

-- | abs.lu: absolute value of -3
tExecStepAbs :: Test
tExecStepAbs =
  "execStep wAbs" ~:
    execStep wAbs initialStore
      ~?= Map.fromList [(globalTableName, Map.fromList [(StringVal "x", IntVal 3)])]

-- | times.lu: multiplication of 3 * 10 by repeated addition
tExecStepTimes :: Test
tExecStepTimes =
  "execStep wTimes" ~:
    execStep wTimes initialStore
      ~?= Map.fromList
        [ ( globalTableName,
            Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 3), (StringVal "z", IntVal 30)]
          )
        ]

-- | fact.lu:  implementation of factorial function (called with argument 5)
tExecStepFact :: Test
tExecStepFact =
  "execStep wFact" ~:
    execStep wFact initialStore
      ~?= Map.fromList
        [ ( globalTableName,
            Map.fromList [(StringVal "f", IntVal 120), (StringVal "n", IntVal 0), (StringVal "x", IntVal 1), (StringVal "z", IntVal 120)]
          )
        ]

-- | table.lu: examples of accessing data from tables
tExecStepTable :: Test
tExecStepTable =
  "execStep wTable" ~:
    execStep wTable initialStore
      ~?= Map.fromList
        [ ( globalTableName,
            Map.fromList
              [ (StringVal "a", TableVal "_t1"),
                (StringVal "k", IntVal 20),
                (StringVal "o1", IntVal 10),
                (StringVal "o2", StringVal "great"),
                (StringVal "o3", IntVal 11)
              ]
          ),
          ("_t1", Map.fromList [(IntVal 20, StringVal "great"), (StringVal "x", IntVal 11)])
        ]

-- | bfs.lu: calculate breadth-first search of a graph represented by adjacency lists.
-- Search for a path from node 1 to node 10
tExecStepBfs :: Test
tExecStepBfs =
  "execStep wBfs" ~:
    TestList
      [ global !? StringVal "found" ~?= Just (BoolVal True)
      ]
  where
    ss = execStep wBfs initialStore
    global = case ss !? globalTableName of
      Just g -> g
      Nothing -> Map.empty

test_execStep :: Test
test_execStep = TestList [tExecStepFact, tExecStepAbs, tExecStepTimes, tExecStepAbs, tExecStepTable, tExecStepBfs]

data Stepper = Stepper
  { filename :: Maybe String,
    block :: Block,
    store :: Store,
    history :: Maybe Stepper
  }

initialStepper :: Stepper
initialStepper =
  Stepper
    { filename = Nothing,
      block = mempty,
      store = initialStore,
      history = Nothing
    }

stepForward :: Stepper -> Stepper
stepForward ss =
  let (curBlock, curStore) = steps 1 (block ss) (store ss)
   in ss {block = curBlock, store = curStore, history = Just ss}

stepForwardN :: Stepper -> Int -> Stepper
stepForwardN ss 0 = ss
stepForwardN ss n =
  let nextStepper = stepForward ss
   in stepForwardN nextStepper (n - 1)

stepBackward :: Stepper -> Maybe Stepper
stepBackward = history

stepBackwardN :: Stepper -> Int -> Maybe Stepper
stepBackwardN ss 0 = Just ss
stepBackwardN ss n =
  let prevStepper = stepBackward ss
   in case prevStepper of
        Just ss' -> stepBackwardN ss' (n - 1)
        _ -> Nothing

-- Fill in `undefined` below
stepper :: IO ()
stepper = go initialStepper
  where
    go :: Stepper -> IO ()
    go ss = do
      prompt ss
      putStr (fromMaybe "Lu" (filename ss) ++ "> ")
      str <- getLine
      case List.uncons (words str) of
        -- load a file for stepping
        Just (":l", [fn]) -> do
          parseResult <- LuParser.parseLuFile fn
          case parseResult of
            (Left _) -> do
              putStr "Failed to parse file"
              go ss
            (Right b) -> do
              putStr ("Loaded " ++ fn ++ ", initializing stepper\n")
              go (ss {filename = Just fn, block = b})
        -- dump the store
        Just (":d", _) -> do
          putStrLn (pretty (store ss))
          go ss
        -- quit the stepper
        Just (":q", _) -> return ()
        -- run current block to completion
        Just (":r", _) ->
          let s' = exec (block ss) (store ss)
           in go ss {block = mempty, store = s', history = Just ss}
        -- next statement (could be multiple)
        Just (":n", strs) -> do
          let numSteps :: Int
              numSteps = case readMaybe (concat strs) of
                Just x -> x
                Nothing -> 1
          let newStepper = stepForwardN ss numSteps
          go newStepper
        -- previous statement
        -- NOTE: this should revert steps of the evaluator not
        -- commands to the stepper. With :n 5 followed by :p
        -- it should back up a single statement, not five statements.
        Just (":p", strs) -> do
          let numSteps :: Int
              numSteps = case readMaybe (concat strs) of
                Just x -> x
                Nothing -> 1

          let newStepper = stepBackwardN ss numSteps
          case newStepper of
            Just ss' -> go ss'
            _ -> do
              putStr "No History to revert..."
              go ss

        -- evaluate an expression in the current state
        _ -> case LuParser.parseLuExp str of
          Right exp -> do
            let v = evaluate exp (store ss)
            putStrLn (pretty v)
            go ss
          Left _s -> do
            putStrLn "?"
            go ss
    prompt :: Stepper -> IO ()
    prompt Stepper {block = Block []} = return ()
    prompt Stepper {block = Block (s : _)} =
      putStr "--> " >> putStrLn (pretty s)

-------------------------- all properties and tests in this module  -----------------------------

test_all :: IO Counts
test_all = runTestTT $ TestList [test_index, test_update, test_resolveVar, test_evaluateNot, test_evaluateLen, test_exec, test_execStep]

-- >>> runTestTT test_all

qc :: IO ()
qc = do
  putStrLn "evalE_total"
  quickCheckN 100 prop_evalE_total
  putStrLn "step_total"
  quickCheckN 100 prop_step_total
  putStrLn "stepExec"
  quickCheckN 100 prop_stepExec
