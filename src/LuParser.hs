module LuParser where

import Control.Applicative
import Data.Char qualified as Char
import LuSyntax
import LuTypes (LType)
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

stringP :: String -> Parser ()
stringP s = wsP (P.string s *> pure ())

constP :: String -> a -> Parser a
constP s x = stringP s *> pure x

afterP :: String -> Parser a -> Parser a
afterP s p = stringP s *> p

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

valueP :: Parser Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP

intValP :: Parser Value
intValP = IntVal <$> wsP P.int

boolValP :: Parser Value
boolValP = BoolVal <$> wsP (trueP <|> falseP)
  where
    trueP :: Parser Bool
    trueP = constP "true" True
    falseP :: Parser Bool
    falseP = constP "false" False


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

nameP :: Parser Name
nameP = P.filter (`notElem` reserved) parseAnyName
  where
    parseAnyName :: Parser Name
    parseAnyName =
      let alphaOrUnderScore = (P.alpha <|> P.char '_')
       in wsP ((:) <$> alphaOrUnderScore <*> many (P.digit <|> alphaOrUnderScore))

uopP :: Parser Uop
uopP = wsP (parseNeg <|> parseLen <|> parseNot)
  where
    parseNeg :: Parser Uop
    parseNeg = constP "-" Neg
    parseLen :: Parser Uop
    parseLen = constP "#" Len
    parseNot :: Parser Uop
    parseNot = constP "not" Not

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

parameterP :: Parser Parameter
parameterP = undefined 

parametersP :: Parser [Parameter]
parametersP = undefined 

lTypeP :: Parser LType 
lTypeP = undefined

functionP :: Parser Value 
functionP = undefined

callP :: Parser Expression
callP = undefined

returnP :: Parser Statement 
returnP = undefined 

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

blockP :: Parser Block
blockP = Block <$> many statementP

parseLuExp :: String -> Either P.ParseError Expression
parseLuExp = P.parse expP

parseLuStat :: String -> Either P.ParseError Statement
parseLuStat = P.parse statementP

parseLuFile :: String -> IO (Either P.ParseError Block)
parseLuFile = P.parseFromFile (const <$> blockP <*> P.eof)