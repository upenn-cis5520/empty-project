module LuStepperTest where 

import LuSyntax
import LuStepper
import LuEvaluator (Store, initialStore, extendedStore, globalTableName, exec)
import State (State)
import State qualified as S
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Test.QuickCheck qualified as QC
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

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

test :: IO Counts
test = runTestTT $ TestList [tExecStepFact, tExecStepAbs, tExecStepTimes, tExecStepAbs, tExecStepTable, tExecStepBfs]

prop_stepExec :: Block -> QC.Property
prop_stepExec b =
  not (final b) QC.==> final b1 QC.==> m1 == m2
  where
    (b1, m1) = S.runState (boundedStep 10 b) initialStore
    m2 = exec b initialStore

-- | Make sure that we can step every block in every store
prop_step_total :: Block -> Store -> Bool
prop_step_total b s = case S.runState (step b) s of
  (b', s') -> True

qc :: IO () 
qc = do 
    putStrLn "stepExec"
    quickCheckN 100 prop_stepExec
    putStrLn "step_total"
    quickCheckN 100 prop_step_total
