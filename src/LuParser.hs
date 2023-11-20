module LuParser where

import Control.Applicative
import Data.Char qualified as Char
import LuSyntax
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
      P.parse (wsP P.alpha) "   " ~?= Left "No parses"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP s = wsP (P.string s *> pure ())

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
      P.parse (stringP "abcd") "ab" ~?= Left "No parses"
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

constP :: String -> a -> Parser a
constP s x = stringP s *> pure x

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
      P.parse (constP "  & " 'a') "  ! " ~?= Left "No parses"
    ]

-- \| parse after a keyword
afterP :: String -> Parser a -> Parser a
afterP s p = stringP s *> p

-- >>> runTestTT test_constP

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

-- >>> P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]"
-- Right [1,1,1]
brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

test_brackets :: Test
test_brackets =
  TestList
    [ P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1],
      P.parse (many (brackets (brackets (constP "1" 1)))) "[[1]] [[  1]]   [1 ]" ~?= Right [1, 1],
      P.parse (many (brackets (constP "1" 1))) "[[1]]" ~?= Right []
    ]

valueP :: Parser Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP

-- >>> P.parse (many intValP) "1 2\n 3"
-- Right [IntVal 1,IntVal 2,IntVal 3]
intValP :: Parser Value
intValP = IntVal <$> wsP P.int

-- >>> P.parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser Value
boolValP = BoolVal <$> wsP (trueP <|> falseP)
  where
    trueP :: Parser Bool
    trueP = constP "true" True
    falseP :: Parser Bool
    falseP = constP "false" False

-- >>> P.parse (many nilValP) "nil nil\n nil"
-- Right [NilVal,NilVal,NilVal]

nilValP :: Parser Value
nilValP = constP "nil" NilVal

quote :: Char
quote = '"'

stringValP :: Parser Value
stringValP = StringVal <$> wsP parseString
  where
    parseString :: Parser String
    parseString =
      P.between (P.string [quote]) (many $ P.satisfy (/= quote)) (P.string [quote])

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

-- >>> runTestTT test_stringValP
-- Counts {cases = 4, tried = 4, errors = 0, failures = 4}

expP :: Parser Expression
expP = compP
  where
    compP = catP `P.chainl1` opAtLevel (level Gt)
    catP = sumP `P.chainl1` opAtLevel (level Concat)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      tableConstP
        <|> Var <$> varP
        <|> parens expP
        <|> Val <$> valueP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- >>>  P.parse (many varP) "x y z"
-- Right [Name "x",Name "y",Name "z"]
-- >>> P.parse varP "(x.y[1]).z"
-- Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z")

varP :: Parser Var
varP = mkVar <$> prefixP <*> some indexP <|> Name <$> nameP
  where
    mkVar :: Expression -> [Expression -> Var] -> Var
    mkVar e l = foldr1 (\f p u -> p (Var (f u))) l e

    prefixP :: Parser Expression
    prefixP = parens expP <|> Var . Name <$> nameP

    indexP :: Parser (Expression -> Var)
    indexP =
      flip Dot <$> (P.string "." *> nameP)
        <|> flip Proj <$> brackets expP

reserved :: [String]
reserved =
  [ "and",
    "break",
    "do",
    "else",
    "elseif",
    "end",
    "false",
    "for",
    "function",
    "goto",
    "if",
    "in",
    "local",
    "nil",
    "not",
    "or",
    "repeat",
    "return",
    "then",
    "true",
    "until",
    "while"
  ]

-- >>> P.parse (many nameP) "x sfds _ nil !bad 2alsobad"
-- Right ["x","sfds","_"]

-- pure ""
nameP :: Parser Name
nameP = P.filter (`notElem` reserved) parseAnyName
  where
    parseAnyName :: Parser Name
    parseAnyName =
      let alphaOrUnderScore = (P.alpha <|> P.char '_')
       in wsP ((:) <$> alphaOrUnderScore <*> many (P.digit <|> alphaOrUnderScore))

tNameP :: Test
tNameP =
  "parse name" ~:
    TestList
      [ P.parse (many nameP) "x sfds _ nil !bad 2alsobad" ~?= Right ["x", "sfds", "_"],
        P.parse (many nameP) "x_x s123 and2 andgood" ~?= Right ["x_x", "s123", "and2", "andgood"],
        P.parse (many nameP) "_12 _34 until" ~?= Right ["_12", "_34"]
      ]

-- >>> P.parse (many uopP) "- - #"
-- Right [Neg,Neg,Len]
uopP :: Parser Uop
uopP = wsP (parseNeg <|> parseLen <|> parseNot)
  where
    parseNeg :: Parser Uop
    parseNeg = constP "-" Neg
    parseLen :: Parser Uop
    parseLen = constP "#" Len
    parseNot :: Parser Uop
    parseNot = constP "not" Not

tuopP :: Test
tuopP =
  "parse tuopP" ~:
    TestList
      [ P.parse (many uopP) "- - #" ~?= Right [Neg, Neg, Len],
        P.parse (many uopP) "not - - # notSomething" ~?= Right [Not, Neg, Neg, Len],
        P.parse (many uopP) "not not not not #" ~?= Right [Not, Not, Not, Not, Len]
      ]

-- >>> P.parse (many bopP) "+ >= .."
-- Right [Plus,Ge,Concat]
bopP :: Parser Bop
bopP =
  constP "+" Plus
    <|> constP "*" Times
    <|> constP "//" Divide
    <|> constP "%" Modulo
    <|> constP "-" Minus
    <|> constP "==" Eq
    <|> constP ">=" Ge
    <|> constP ">" Gt
    <|> constP "<=" Le
    <|> constP "<" Lt
    <|> constP ".." Concat

test_bopP :: Test
test_bopP =
  "Parsing bopP" ~:
    TestList
      [ P.parse (many bopP) "+ - * // % ==" ~?= Right [Plus, Minus, Times, Divide, Modulo, Eq],
        P.parse (many bopP) "> >= < <= .." ~?= Right [Gt, Ge, Lt, Le, Concat],
        P.parse (many bopP) "> >= $ <=" ~?= Right [Gt, Ge],
        P.parse (many bopP) ">=.. <= ..>..<" ~?= Right [Ge, Concat, Le, Concat, Gt, Concat, Lt]
      ]

-- >>> P.parse tableConstP "{ x = 2, [3] = false }"
-- Right (TableConst [FieldName "x" (Val (IntVal 2)),FieldKey (Val (IntVal 3)) (Val (BoolVal False))])
tableConstP :: Parser Expression
tableConstP = TableConst <$> braces (P.sepBy fieldP (wsP (P.char ',')))
  where
    fieldP :: Parser TableField
    fieldP = fieldNameP <|> fieldKeyP
      where
        fieldNameP :: Parser TableField
        fieldNameP = liftA2 FieldName nameP (afterP "=" expP)
        fieldKeyP :: Parser TableField
        fieldKeyP = liftA2 FieldKey (brackets expP) (afterP "=" expP)

test_tableConstP :: Test
test_tableConstP =
  "Parsing tableConst" ~:
    TestList
      [ P.parse tableConstP "{ x = 2, [3] = false }" ~?= Right (TableConst [FieldName "x" (Val (IntVal 2)), FieldKey (Val (IntVal 3)) (Val (BoolVal False))]),
        P.parse tableConstP "{ abc = 3, [2] = true, [4] = false, [9] = \"here\"}" ~?= Right (TableConst [FieldName "abc" (Val (IntVal 3)), FieldKey (Val (IntVal 2)) (Val (BoolVal True)), FieldKey (Val (IntVal 4)) (Val (BoolVal False)), FieldKey (Val (IntVal 9)) (Val (StringVal "here"))])
      ]

statementP :: Parser Statement
statementP = wsP (assignP <|> ifP <|> whileP <|> emptyP <|> repeatP)
  where
    assignP :: Parser Statement
    assignP = Assign <$> varP <*> (stringP "=" *> expP)
    ifP :: Parser Statement
    ifP = liftA3 If (afterP "if" expP) (afterP "then" blockP) (afterP "else" blockP) <* stringP "end"
    whileP :: Parser Statement
    whileP = liftA2 While (afterP "while" expP) (afterP "do" blockP) <* stringP "end"
    emptyP :: Parser Statement
    emptyP = constP ";" Empty
    repeatP :: Parser Statement
    repeatP = liftA2 Repeat (afterP "repeat" blockP) (afterP "until" expP)

-- Repeat <$> (stringP "repeat" *> blockP) <*> (stringP "until" *> expP)

blockP :: Parser Block
blockP = Block <$> many statementP

parseLuExp :: String -> Either P.ParseError Expression
parseLuExp = P.parse expP

parseLuStat :: String -> Either P.ParseError Statement
parseLuStat = P.parse statementP

parseLuFile :: String -> IO (Either P.ParseError Block)
parseLuFile = P.parseFromFile (const <$> blockP <*> P.eof)

tParseFiles :: Test
tParseFiles =
  "parse files" ~:
    TestList
      [ "fact" ~: p "lu/fact.lu" wFact,
        "test" ~: p "lu/test.lu" wTest,
        "abs" ~: p "lu/abs.lu" wAbs,
        "times" ~: p "lu/times.lu" wTimes,
        "table" ~: p "lu/table.lu" wTable,
        "bfs" ~: p "lu/bfs.lu" wBfs
      ]
  where
    p fn ast = do
      result <- parseLuFile fn
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

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

-- >>> runTestTT test_stat

test_all :: IO Counts
test_all = runTestTT $ TestList [test_comb, test_value, test_exp, test_stat, tParseFiles]

-- >>> test_all

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_stat"
  QC.quickCheck prop_roundtrip_stat
