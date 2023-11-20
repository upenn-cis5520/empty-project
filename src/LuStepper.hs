module LuStepper where

import Control.Monad (when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import LuParser qualified
import LuSyntax
import State (State)
import State qualified as S
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)

type Store = Map Name Table

type Table = Map Value Value

globalTableName :: Name
globalTableName = "_G"

initialStore :: Store
initialStore = Map.singleton globalTableName Map.empty

extendedStore :: Store
extendedStore =
  Map.fromList
    [ ( globalTableName,
        Map.fromList
          [ (StringVal "x", IntVal 3),
            (StringVal "t", TableVal "_t1")
          ]
      ),
      ( "_t1",
        Map.fromList
          [ (StringVal "y", BoolVal True),
            (IntVal 2, TableVal "_t1")
          ]
      )
    ]

-- >>> oneLine initialStore
-- "{_G = {}}"

-- >>> oneLine extendedStore
-- "{_G = {t = <_t1> x = 3} _t1 = {[2] = <_t1> y = true}}"

type Reference = (Name, Value)

xref :: Reference
xref = ("_G", StringVal "x")

yref :: Reference
yref = ("_t1", StringVal "y")

tableFromState :: Name -> State Store (Maybe Table)
tableFromState tname = Map.lookup tname <$> S.get

index :: Reference -> State Store Value
index (tableName, key) = do
  t <- tableFromState tableName
  case t of
    Just t -> case Map.lookup key t of
      Just v -> return v
      _ -> return NilVal
    _ -> return NilVal

test_index :: Test
test_index =
  "index tests" ~:
    TestList
      [ -- The global variable "x" is unitialized (i.e. not present in the initial store)
        S.evalState (index xref) initialStore ~?= NilVal,
        -- But there is a value for "x" available in the extended store
        S.evalState (index xref) extendedStore ~?= IntVal 3,
        -- If a table name is not found in the store, accessing its reference also returns nil.
        S.evalState (index yref) initialStore ~?= NilVal,
        -- We should also be able to access "t[1]" in the extended store
        S.evalState (index yref) extendedStore ~?= BoolVal True,
        S.evalState (index ("z", NilVal)) extendedStore ~?= NilVal,
        -- Updates using the `nil` key are ignored
        S.execState (update ("_t1", NilVal) (IntVal 3)) extendedStore ~?= extendedStore,
        S.evalState (index (globalTableName, StringVal "t")) extendedStore ~?= TableVal "_t1"
      ]

-- >>> runTestTT test_index

update :: Reference -> Value -> State Store ()
update (tableName, NilVal) newVal = S.get >>= \s -> return ()
update (tableName, key) newVal = do
  t <- tableFromState tableName
  S.modify (updateStore t)
  where
    updateStore :: Maybe Table -> (Store -> Store)
    updateStore maybeTable =
      case maybeTable of
        Just t -> Map.insert tableName (Map.insert key newVal t)
        _ -> id

test_update :: Test
test_update =
  "index tests" ~:
    TestList
      [ -- If we assign to x, then we can find its new value
        S.evalState (update xref (IntVal 4) >> index xref) initialStore ~?= IntVal 4,
        -- If we assign to x, then remove it, we cannot find it anymore
        S.evalState (update xref (IntVal 4) >> update xref NilVal >> index xref) initialStore ~?= NilVal,
        -- If we assign to t.y, then we can find its new value
        S.evalState (update yref (IntVal 5) >> index yref) extendedStore ~?= IntVal 5,
        -- If we assign nil to t.y, then we cannot find it
        S.evalState (update yref NilVal >> index yref) extendedStore ~?= NilVal
      ]

-- >>> runTestTT test_update

allocateTable :: [(Value, Value)] -> State Store Value
allocateTable assocs = do
  store <- S.get
  -- make a fresh name for the new table
  let n = length (Map.keys store)
  let tableName = "_t" ++ show n
  -- make sure we don't have a nil key or value
  let assocs' = filter nonNil assocs
  -- update the store
  S.put (Map.insert tableName (Map.fromList assocs') store)
  return (TableVal tableName)

-- Keep nil out of the table
nonNil :: (Value, Value) -> Bool
nonNil (k, v) = k /= NilVal && v /= NilVal

-- | Convert a variable into a reference into the store.
-- Fails when the var is `t.x` or t[1] and `t` is not defined in the store
-- when the var is `2.y` or `nil[2]` (i.e. not a `TableVal`)
-- or when the var is t[nil]
resolveVar :: Var -> State Store (Maybe Reference)
resolveVar (Name n) = do
  mGlobalTable <- tableFromState globalTableName
  return $ case mGlobalTable of
    Just globalTable -> Just (globalTableName, StringVal n)
    _ -> Nothing
resolveVar (Dot exp n) = do
  mGlobalTable <- tableFromState globalTableName
  e <- evalE exp
  return $ case (e, mGlobalTable) of
    (TableVal tname, Just globalTable) -> Just (tname, StringVal n)
    _ -> Nothing
resolveVar (Proj exp1 exp2) = do
  e1 <- evalE exp1
  e2 <- evalE exp2
  return $ case (e1, e2) of
    (_, NilVal) -> Nothing
    (TableVal t1, v) -> Just (t1, v)
    _ -> Nothing

test_resolveVar :: Test
test_resolveVar =
  "resolveVar" ~:
    TestList
      [ -- we should be able to resolve global variable `x` in the initial store, even though it is not defined
        S.evalState (resolveVar (Name "x")) initialStore ~?= Just ("_G", StringVal "x"),
        S.evalState (resolveVar (Name "x")) extendedStore ~?= Just ("_G", StringVal "x"),
        -- But in the case of Dot or Proj, the first argument should evaluate to a
        -- TableVal that is defined in the store. If it does not, then resolveVar
        -- should return Nothing.
        S.evalState (resolveVar (Dot (Val NilVal) "x")) initialStore ~?= Nothing,
        S.evalState (resolveVar (Dot (Var (Name "t")) "x")) initialStore ~?= Nothing,
        -- For Proj we also shouldn't project from Nil
        S.evalState (resolveVar (Proj (Var (Name "t")) (Val NilVal))) extendedStore ~?= Nothing,
        -- If the table is defined, we should return a reference to it, even when the field is undefined
        S.evalState (resolveVar (Dot (Var (Name "t")) "z")) extendedStore ~?= Just ("_t1", StringVal "z"),
        S.evalState (resolveVar (Dot (Var (Name "t")) "y")) extendedStore ~?= Just ("_t1", StringVal "y"),
        S.evalState (resolveVar (Dot (Var (Name "t2")) "z")) extendedStore ~?= Nothing,
        -- and how we refer to the field shouldn't matter
        S.evalState (resolveVar (Proj (Var (Name "t")) (Val (StringVal "z")))) extendedStore ~?= Just ("_t1", StringVal "z"),
        S.evalState (resolveVar (Proj (Var (Name "t2")) (Val (StringVal "z")))) extendedStore ~?= Nothing,
        S.evalState (resolveVar (Proj (Val NilVal) (Val (StringVal "z")))) extendedStore ~?= Nothing
      ]

-- >>> runTestTT test_resolveVar

-- | Expression evaluator
evalE :: Expression -> State Store Value
evalE (Var v) = do
  mr <- resolveVar v -- see above
  case mr of
    Just r -> index r
    Nothing -> return NilVal
evalE (Val v) = return v
evalE (Op2 e1 o e2) = evalOp2 o <$> evalE e1 <*> evalE e2
evalE (Op1 o e) = do
  s <- S.get
  e' <- evalE e
  evalOp1 o e'
evalE (TableConst _fs) = evalTableConst _fs

fieldToPair :: TableField -> State Store (Value, Value)
fieldToPair (FieldName n exp) = do
  e <- evalE exp
  return (StringVal n, e)
fieldToPair (FieldKey exp1 exp2) = do
  e1 <- evalE exp1
  e2 <- evalE exp2
  return (e1, e2)

accuFieldToPair :: TableField -> State Store [(Value, Value)] -> State Store [(Value, Value)]
accuFieldToPair tf accu = do
  pair <- fieldToPair tf
  rest <- accu
  return (pair : rest)

evalTableConst :: [TableField] -> State Store Value
evalTableConst xs = do
  vs <- helper xs
  allocateTable vs
  where
    helper :: [TableField] -> State Store [(Value, Value)]
    helper = foldr accuFieldToPair (return [])

getTableSizeState :: String -> State Store (Maybe Int)
getTableSizeState v =
  S.get >>= \s -> return $ do
    targetTable <- Map.lookup v s
    return $ length targetTable

evalOp1 :: Uop -> Value -> State Store Value
evalOp1 Neg (IntVal v) = return $ IntVal $ negate v
evalOp1 Len (StringVal v) = return $ IntVal $ length v
evalOp1 Len (TableVal v) = do
  ml <- getTableSizeState v
  return $ case ml of
    Just l -> IntVal l
    _ -> NilVal
evalOp1 Len iv@(IntVal v) = return iv
evalOp1 Len (BoolVal True) = return $ IntVal 1
evalOp1 Len (BoolVal False) = return $ IntVal 0
evalOp1 Not v = return $ BoolVal $ not $ toBool v
evalOp1 _ _ = return NilVal

evalOp2 :: Bop -> Value -> Value -> Value
evalOp2 Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalOp2 Divide (IntVal _) (IntVal 0) = NilVal
evalOp2 Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
evalOp2 Modulo (IntVal i1) (IntVal 0) = NilVal
evalOp2 Modulo (IntVal i1) (IntVal i2) = IntVal $ i1 `mod` i2
evalOp2 Eq v1 v2 = BoolVal $ v1 == v2
evalOp2 Gt v1 v2 = BoolVal $ v1 > v2
evalOp2 Ge v1 v2 = BoolVal $ v1 >= v2
evalOp2 Lt v1 v2 = BoolVal $ v1 < v2
evalOp2 Le v1 v2 = BoolVal $ v1 <= v2
evalOp2 Concat (StringVal s1) (StringVal s2) = StringVal (s1 ++ s2)
evalOp2 _ _ _ = NilVal

evaluate :: Expression -> Store -> Value
evaluate e = S.evalState (evalE e)

-- | Determine whether a value should be interpreted as true or false when
-- used as a condition.
toBool :: Value -> Bool
toBool (BoolVal False) = False
toBool NilVal = False
toBool _ = True

test_evaluateNot :: Test
test_evaluateNot =
  "evaluate not" ~:
    TestList
      [ evaluate (Op1 Not (Val NilVal)) initialStore ~?= BoolVal True,
        evaluate (Op1 Not (Val (IntVal 3))) initialStore ~?= BoolVal False
      ]

-- >>> runTestTT test_evaluateNot

test_evaluateLen :: Test
test_evaluateLen =
  "evaluate len" ~:
    TestList
      [ evaluate (Op1 Len (Val (StringVal "5520"))) extendedStore ~?= IntVal 4,
        evaluate (Op1 Len (Val (TableVal "_G"))) extendedStore ~?= IntVal 2,
        evaluate (Op1 Len (Val (TableVal "_t1"))) extendedStore ~?= IntVal 2,
        evaluate (Op1 Len (Val (TableVal "_t550"))) extendedStore ~?= NilVal,
        evaluate (Op1 Len (Val (IntVal 5520))) extendedStore ~?= IntVal 5520,
        evaluate (Op1 Len (Val (BoolVal True))) extendedStore ~?= IntVal 1,
        evaluate (Op1 Len (Val NilVal)) extendedStore ~?= NilVal
      ]

-- >>> runTestTT test_evaluateLen

test_tableConst :: Test
test_tableConst =
  "evaluate { x = 3 } " ~:
    TestList
      [ S.runState
          (evalE (TableConst [FieldName "x" (Val (IntVal 3))]))
          initialStore
          ~?= ( TableVal "_t1",
                Map.fromList
                  [ ("_G", Map.empty),
                    ("_t1", Map.fromList [(StringVal "x", IntVal 3)])
                  ]
              ),
        S.runState
          (evalE (TableConst [FieldName "x" (Val (IntVal 3)), FieldName "y" (Val (IntVal 5))]))
          initialStore
          ~?= ( TableVal "_t1",
                Map.fromList
                  [ ("_G", Map.empty),
                    ("_t1", Map.fromList [(StringVal "x", IntVal 3), (StringVal "y", IntVal 5)])
                  ]
              ),
        S.runState
          (evalE (TableConst [FieldKey (Val (StringVal "x")) (Val (IntVal 3))]))
          initialStore
          ~?= ( TableVal "_t1",
                Map.fromList
                  [ ("_G", Map.empty),
                    ("_t1", Map.fromList [(StringVal "x", IntVal 3)])
                  ]
              ),
        S.runState
          (evalE (TableConst [FieldKey (Val (StringVal "x")) (Val (IntVal 3)), FieldName "y" (Val (IntVal 5))]))
          initialStore
          ~?= ( TableVal "_t1",
                Map.fromList
                  [ ("_G", Map.empty),
                    ("_t1", Map.fromList [(StringVal "x", IntVal 3), (StringVal "y", IntVal 5)])
                  ]
              ),
        S.runState
          (evalE (TableConst []))
          initialStore
          ~?= ( TableVal "_t1",
                Map.fromList
                  [ ("_G", Map.empty),
                    ("_t1", Map.empty)
                  ]
              )
      ]

test_evalOp2 :: Test
test_evalOp2 =
  "evaluate Op2" ~:
    TestList
      [ evaluate (Op2 (Val (IntVal 3)) Plus (Val (IntVal 1))) initialStore ~?= IntVal 4,
        evaluate (Op2 (Val (IntVal 3)) Minus (Val (IntVal 1))) initialStore ~?= IntVal 2,
        evaluate (Op2 (Val (IntVal 3)) Times (Val (IntVal 1))) initialStore ~?= IntVal 3,
        evaluate (Op2 (Val (IntVal 4)) Divide (Val (IntVal 2))) initialStore ~?= IntVal 2,
        evaluate (Op2 (Val (IntVal 3)) Divide (Val (IntVal 0))) initialStore ~?= NilVal,
        evaluate (Op2 (Val (IntVal 3)) Modulo (Val (IntVal 0))) initialStore ~?= NilVal,
        evaluate (Op2 (Val (IntVal 3)) Modulo (Val (IntVal 2))) initialStore ~?= IntVal 1,
        evaluate (Op2 (Val (IntVal 3)) Eq (Val (IntVal 2))) initialStore ~?= BoolVal False,
        evaluate (Op2 (Val (IntVal 3)) Eq (Val (IntVal 3))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Gt (Val (IntVal 2))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Gt (Val (IntVal 3))) initialStore ~?= BoolVal False,
        evaluate (Op2 (Val (IntVal 3)) Gt (Val (IntVal 4))) initialStore ~?= BoolVal False,
        evaluate (Op2 (Val (IntVal 3)) Ge (Val (IntVal 3))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Ge (Val (IntVal 2))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Ge (Val (IntVal 4))) initialStore ~?= BoolVal False,
        evaluate (Op2 (Val (IntVal 3)) Lt (Val (IntVal 2))) initialStore ~?= BoolVal False,
        evaluate (Op2 (Val (IntVal 3)) Lt (Val (IntVal 3))) initialStore ~?= BoolVal False,
        evaluate (Op2 (Val (IntVal 3)) Lt (Val (IntVal 4))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Le (Val (IntVal 3))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Le (Val (IntVal 2))) initialStore ~?= BoolVal False,
        evaluate (Op2 (Val (IntVal 3)) Ge (Val (IntVal 3))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Ge (Val (IntVal 2))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Ge (Val (IntVal 4))) initialStore ~?= BoolVal False,
        evaluate (Op2 (Val (IntVal 3)) Ge (Val (IntVal 3))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 3)) Ge (Val (IntVal 2))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (StringVal "hello ")) Concat (Val (StringVal "world!"))) initialStore ~?= StringVal "hello world!"
      ]

-- >>> runTestTT test_tableConst

prop_evalE_total :: Expression -> Store -> Bool
prop_evalE_total e s = case evaluate e s of
  NilVal -> True
  IntVal i -> i `seq` True
  BoolVal b -> b `seq` True
  StringVal s -> s `seq` True
  TableVal n -> n `seq` True

eval :: Block -> State Store ()
eval (Block ss) = mapM_ evalS ss

-- | Statement evaluator
evalS :: Statement -> State Store ()
evalS (If e s1 s2) = do
  v <- evalE e
  if toBool v then eval s1 else eval s2
evalS w@(While e ss) = do
  v <- evalE e
  when (toBool v) $ do
    eval ss
    evalS w
evalS (Assign v e) = do
  -- update global variable or table field v to value of e
  s <- S.get
  mRef <- resolveVar v
  e' <- evalE e
  case mRef of
    Just ref -> update ref e'
    _ -> return ()
evalS s@(Repeat b e) = evalS (While (Op1 Not e) b) -- keep evaluating block b until expression e is true
evalS Empty = return () -- do nothing

exec :: Block -> Store -> Store
exec = S.execState . eval

-- >>> runTestTT test_exec

-------------------------- Test cases for exec -----------------------------

tExecTest :: Test
tExecTest =
  "exec wTest" ~:
    exec wTest initialStore
      ~?= Map.fromList [(globalTableName, Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 10)])]

tExecFact :: Test
tExecFact =
  "exec wFact" ~:
    exec wFact initialStore
      ~?= Map.fromList
        [ ( globalTableName,
            Map.fromList
              [ (StringVal "f", IntVal 120),
                (StringVal "n", IntVal 0),
                (StringVal "x", IntVal 1),
                (StringVal "z", IntVal 120)
              ]
          )
        ]

tExecAbs :: Test
tExecAbs =
  "exec wAbs" ~:
    exec wAbs initialStore
      ~?= Map.fromList
        [ ( globalTableName,
            Map.fromList [(StringVal "x", IntVal 3)]
          )
        ]

tExecTimes :: Test
tExecTimes =
  "exec wTimes" ~:
    exec wTimes initialStore
      ~?= Map.fromList
        [ ( globalTableName,
            Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 3), (StringVal "z", IntVal 30)]
          )
        ]

tExecTable :: Test
tExecTable =
  "exec wTable" ~:
    exec wTable initialStore
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

tExecBfs :: Test
tExecBfs = "exec wBfs" ~: TestList [global !? StringVal "found" ~?= Just (BoolVal True)]
  where
    ss = exec wBfs initialStore
    global = case ss !? globalTableName of
      Just g -> g
      Nothing -> Map.empty

test_exec :: Test
test_exec = TestList [tExecTest, tExecFact, tExecAbs, tExecTimes, tExecTable, tExecBfs]

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
