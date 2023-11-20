module LuParserTest where 

import Control.Applicative
import Parser qualified as P
import Parser (Parser)
import LuSyntax
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
          ~?= Right (Repeat (Block [Empty, Empty]) (Val (BoolVal False)))
      ]

test :: IO Counts
test = runTestTT $ TestList [test_wsP, test_stringP, test_constP, test_brackets, test_stringValP, test_nameP, test_uopP, test_bopP, test_tableConstP, test_ParseFiles, test_comb, test_value, test_exp, test_stat]

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