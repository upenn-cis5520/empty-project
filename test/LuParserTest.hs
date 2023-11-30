module LuParserTest where 

import Control.Applicative
import Parser qualified as P
import Parser (Parser)
import LuSyntax
import LuTypes
import Test.QuickCheck qualified as QC
import LuParser as LP
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (LP.wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (LP.wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
      P.parse (LP.wsP P.alpha) "   " ~?= Left "No parses"
    ]

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
      P.parse (stringP "abcd") "ab" ~?= Left "No parses"
    ]

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
      P.parse (constP "  & " 'a') "  ! " ~?= Left "No parses"
    ]

test_brackets :: Test
test_brackets =
  TestList
    [ P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1],
      P.parse (many (brackets (brackets (constP "1" 1)))) "[[1]] [[  1]]   [1 ]" ~?= Right [1, 1],
      P.parse (many (brackets (constP "1" 1))) "[[1]]" ~?= Right []
    ]

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"],
      P.parse (many stringValP) "\"a   \"  \"   c\"" ~?= Right [StringVal "a   ", StringVal "   c"],
      P.parse (many stringValP) "\"a   \" \"  \"  \"   c\"" ~?= Right [StringVal "a   ", StringVal "  ", StringVal "   c"]
    ]

test_nameP :: Test
test_nameP =
  "parse name" ~:
    TestList
      [ P.parse (many nameP) "x sfds _ nil !bad 2alsobad" ~?= Right ["x", "sfds", "_"],
        P.parse (many nameP) "x_x s123 and2 andgood" ~?= Right ["x_x", "s123", "and2", "andgood"],
        P.parse (many nameP) "_12 _34 until" ~?= Right ["_12", "_34"]
      ]

test_uopP :: Test
test_uopP =
  "parse uopP" ~:
    TestList
      [ P.parse (many uopP) "- - #" ~?= Right [Neg, Neg, Len],
        P.parse (many uopP) "not - - # notSomething" ~?= Right [Not, Neg, Neg, Len, Not],
        P.parse (many uopP) "not not not not #" ~?= Right [Not, Not, Not, Not, Len]
      ]

test_bopP :: Test
test_bopP =
  "Parsing bopP" ~:
    TestList
      [ P.parse (many bopP) "+ - * // % ==" ~?= Right [Plus, Minus, Times, Divide, Modulo, Eq],
        P.parse (many bopP) "> >= < <= .." ~?= Right [Gt, Ge, Lt, Le, Concat],
        P.parse (many bopP) "> >= $ <=" ~?= Right [Gt, Ge],
        P.parse (many bopP) ">=.. <= ..>..<" ~?= Right [Ge, Concat, Le, Concat, Gt, Concat, Lt]
      ]
-- START NEW TESTS
test_functionP :: Test 
test_functionP = 
  "Parsing functionP" ~: 
    TestList 
     [P.parse functionP "function (x: int): nil ; end " ~?= Right (FunctionVal [("x", IntType)] NilType (Block [Empty])), 
      P.parse functionP "function (x: boolean, y: boolean): boolean return x end" ~?= Right (FunctionVal [("x", BooleanType), ("y", BooleanType)] BooleanType (Block [Return (Var (Name "x"))])), 
      P.parse functionP "function (): string return \"hello world\" end" ~?= Right (FunctionVal [] StringType (Block [Return (Val (StringVal "hello world"))]))
     ] 

test_callP :: Test 
test_callP = 
  "Parsing callP" ~: 
    TestList 
      [P.parse callP "foo()" ~?= Right (Call (Name "foo") []), 
       P.parse callP "foo(x, y)" ~?= Right (Call (Name "foo") [Var (Name "x"), Var (Name "y")]), 
       P.parse callP "foo(1+1)" ~?= Right (Call (Name "foo") [Op2 (Val (IntVal 1)) Plus (Val (IntVal 1))])] 

test_parameterP :: Test
test_parameterP = 
  "Parsing parameterP" ~:
    TestList 
      [P.parse parameterP "x: int" ~?= Right ("x", IntType), 
       P.parse parameterP "y: string" ~?= Right ("y", StringType), 
       P.parse parameterP "z: boolean" ~?= Right ("z", BooleanType)] 

test_parametersP :: Test
test_parametersP = 
  "Parsing parametersP" ~: 
    TestList 
      [P.parse parametersP "(x: int, y: int, z: int)" ~?= Right [("x", IntType), ("y", IntType), ("z", IntType)], 
       P.parse parametersP "(x: int, y: string, z: boolean)" ~?= Right [("x", IntType), ("y", StringType), ("z", BooleanType)],
       P.parse parametersP "()" ~?= Right []
      ] 

test_lTypeP :: Test 
test_lTypeP = 
  "Parsing lTypeP" ~:
    TestList 
      [P.parse lTypeP "int " ~?= Right IntType, 
       P.parse lTypeP "nil  " ~?= Right NilType, 
       P.parse lTypeP "string" ~?= Right StringType, 
       P.parse lTypeP "boolean" ~?= Right BooleanType, 
       P.parse (many lTypeP) "int boolean frog" ~?= Right [IntType, BooleanType], 
       P.parse (many lTypeP) "string string   string turtle" ~?= Right [StringType, StringType, StringType]]

test_returnP :: Test 
test_returnP = 
  "Parsing returnP" ~: 
    TestList 
     [
      P.parse returnP "return x+5" ~?= Right (Return (Op2 (Var (Name "x")) Plus (Val (IntVal 5)))), 
      P.parse returnP "return 0" ~?= Right (Return (Val (IntVal 0))), 
      P.parse returnP "return not true" ~?= Right (Return (Op1 Not (Val (BoolVal True)))) 
     ]
-- END NEW TESTS
test_tableConstP :: Test
test_tableConstP =
  "Parsing tableConst" ~:
    TestList
      [ P.parse tableConstP "{ x = 2, [3] = false }" ~?= Right (TableConst [FieldName "x" (Val (IntVal 2)), FieldKey (Val (IntVal 3)) (Val (BoolVal False))]),
        P.parse tableConstP "{ abc = 3, [2] = true, [4] = false, [9] = \"here\"}" ~?= Right (TableConst [FieldName "abc" (Val (IntVal 3)), FieldKey (Val (IntVal 2)) (Val (BoolVal True)), FieldKey (Val (IntVal 4)) (Val (BoolVal False)), FieldKey (Val (IntVal 9)) (Val (StringVal "here"))])
      ]

test_ParseFiles :: Test
test_ParseFiles =
  "parse files" ~:
    TestList
      [ "fact" ~: p "test/lu/fact.lu" wFact,
        "test" ~: p "test/lu/test.lu" wTest,
        "abs" ~: p "test/lu/abs.lu" wAbs,
        "times" ~: p "test/lu/times.lu" wTimes,
        "table" ~: p "test/lu/table.lu" wTable,
        "bfs" ~: p "test/lu/bfs.lu" wBfs
      ]
  where
    p fn ast = do
      result <- parseLuFile fn
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

test_comb :: Test
test_comb =
  "parsing combinators" ~:
    TestList
      [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
        P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
        P.parse (stringP "a") "a" ~?= Right (),
        P.parse (stringP "a") "b" ~?= Left "No parses",
        P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
        P.parse (constP "&" 'a') "&  " ~?= Right 'a',
        P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
        P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1]
      ]

test_value :: Test
test_value =
  "parsing values" ~:
    TestList
      [ P.parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
        P.parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
        P.parse (many nilValP) "nil nil\n nil" ~?= Right [NilVal, NilVal, NilVal],
        P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
        P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
        P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
        P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
      ]

test_exp :: Test
test_exp =
  "parsing expressions" ~:
    TestList
      [ P.parse (many varP) "x y z" ~?= Right [Name "x", Name "y", Name "z"],
        P.parse varP "(x.y[1]).z" ~?= Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z"),
        P.parse (many nameP) "x sfds _ nil" ~?= Right ["x", "sfds", "_"],
        P.parse (many uopP) "- - #" ~?= Right [Neg, Neg, Len],
        P.parse (many bopP) "+ >= .." ~?= Right [Plus, Ge, Concat],
        P.parse tableConstP "{ x = 2, [3] = false }"
          ~?= Right (TableConst [FieldName "x" (Val (IntVal 2)), FieldKey (Val (IntVal 3)) (Val (BoolVal False))])
      ]
-- NEW TESTS
test_stat :: Test
test_stat =
  "parsing statements" ~:
    TestList
      [ P.parse statementP ";" ~?= Right Empty,
        P.parse statementP "x=3" ~?= Right (Assign (Name "x") (Val (IntVal 3))),
        P.parse statementP "if x then y=nil else end"
          ~?= Right (If (Var (Name "x")) (Block [Assign (Name "y") (Val NilVal)]) (Block [])),
        P.parse statementP "while nil do end"
          ~?= Right (While (Val NilVal) (Block [])),
        P.parse statementP "repeat ; ; until false"
          ~?= Right (Repeat (Block [Empty, Empty]) (Val (BoolVal False))), 
        P.parse statementP "function foo(x: int): int return x + 5 end" ~?= Right (Assign (Name "foo") (Val (FunctionVal [("x", IntType)] IntType (Block [Return (Op2 (Var (Name "x")) Plus (Val (IntVal 5)))])))), 
        P.parse statementP "function foo(): nil ; end" ~?= Right (Assign (Name "foo") (Val (FunctionVal [] NilType (Block [Empty])))), 
        P.parse statementP "function foo(x: int, y: int): string return \"here\" end" ~?= Right (Assign (Name "foo") (Val (FunctionVal [("x", IntType), ("y", IntType)] StringType (Block [Return (Val (StringVal "here"))]))))
      ]

test :: IO Counts
test = runTestTT $ TestList [test_wsP, test_stringP, test_constP, test_brackets, test_stringValP, test_nameP, test_uopP, test_bopP, test_functionP, test_returnP, test_callP, test_tableConstP, test_parameterP, test_parametersP, test_lTypeP, test_ParseFiles, test_comb, test_value, test_exp, test_stat]

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_stat"
  QC.quickCheck prop_roundtrip_stat