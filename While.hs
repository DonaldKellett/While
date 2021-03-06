module While where

import Text.Parsec
import Control.Monad
import Data.Either.Extra
import qualified Data.Map.Strict as Map

{-
Abstract syntax and operational semantics of While
-}

-- Arithmetic expressions
data AExp =
  AId String |       -- Variables
  ANum Integer |     -- Integer Literals
  APlus AExp AExp |  -- Addition
  AMinus AExp AExp | -- Subtraction
  AMult AExp AExp |  -- Multiplication
  ADiv AExp AExp     -- Integer Divison
  deriving (Eq, Show)

-- Boolean expressions
data BExp =
  BTrue |          -- true
  BFalse |         -- false
  BAnd BExp BExp | -- boolean AND
  BOr BExp BExp |  -- boolean OR
  BGt AExp AExp |  -- arithmetic greater than
  BLt AExp AExp    -- arithmetic less than
  deriving (Eq, Show)

-- Statements
data Stmt =
  SAss String AExp |   -- Variable assignment
  SSeq Stmt Stmt |     -- Statement sequencing
  SIf BExp Stmt Stmt | -- Conditional statement
  SWhile BExp Stmt     -- While loop
  deriving (Eq, Show)

-- Program state
type St = Map.Map String Integer

-- Evaluation of arithmetic expressions
aeval :: St -> AExp -> Maybe Integer
aeval st a = case a of
  AId x -> Map.lookup x st
  ANum n -> return n
  APlus a1 a2 -> liftM2 (+) (aeval st a1) (aeval st a2)
  AMinus a1 a2 -> liftM2 (-) (aeval st a1) (aeval st a2)
  AMult a1 a2 -> liftM2 (*) (aeval st a1) (aeval st a2)
  ADiv a1 a2 -> do
    n1 <- aeval st a1
    n2 <- aeval st a2
    if n2 == 0 then Nothing else return (div n1 n2)

-- Evaluation of boolean expressions
beval :: St -> BExp -> Maybe Bool
beval st b = case b of
  BTrue -> return True
  BFalse -> return False
  BAnd b1 b2 -> liftM2 (&&) (beval st b1) (beval st b2)
  BOr b1 b2 -> liftM2 (||) (beval st b1) (beval st b2)
  BGt a1 a2 -> liftM2 (>) (aeval st a1) (aeval st a2)
  BLt a1 a2 -> liftM2 (<) (aeval st a1) (aeval st a2)

-- Evaluation of statements
seval :: St -> Stmt -> Maybe St
seval st s = case s of
  SAss x a -> do
    n <- aeval st a
    return (Map.insert x n st)
  SSeq s1 s2 -> do
    st' <- seval st s1
    seval st' s2
  SIf b s1 s2 -> do
    cond <- beval st b
    if cond then seval st s1 else seval st s2
  SWhile b s' -> do
    cond <- beval st b
    if cond
      then do
        st' <- seval st s'
        seval st' s
      else return st

{-
Parsers for translating concrete syntax of While into abstract syntax
-}

-- A variable consists of 1-10 lowercase letters and cannot be a reserved keyword
var :: Parsec String () String
var = do
  x <- many1 lower
  guard (length x <= 10)
  guard (not (elem x ["and", "or", "true", "false", "if", "then", "else", "while", "do"]))
  return x

-- An arithmetic expression is a variable, numeral, arithmetic operator applied to two arithmetic expressions or a
-- parenthesized arithmetic expression
-- In order to make our parser respect the order of operations, we further subdivide expressions into factors (variable,
-- numeral or parenthesized expression) and terms (factors seperated by multiplication and/or division only)
aexp :: Parsec String () AExp
aexp = spaces *> chainl1 term plusOrMinus <* spaces
  where
    term = spaces *> chainl1 factor (try multOrDiv) <* spaces
    factor = try (between (char '(') (char ')') aexp) <|> try (AId <$> var) <|> num
    num = try posNum <|> try zeroNum <|> negNum
    posNum = do
      leadDigit <- oneOf "123456789"
      otherDigits <- many digit
      return (ANum (read (leadDigit : otherDigits)))
    zeroNum = do
      char '0'
      return (ANum 0)
    negNum = do
      char '-'
      ANum absN <- posNum
      return (ANum (-absN))
    plusOrMinus = do
      spaces
      symbol <- oneOf "+-"
      spaces
      case symbol of
        '+' -> return APlus
        '-' -> return AMinus
    multOrDiv = do
      spaces
      symbol <- oneOf "*/"
      spaces
      case symbol of
        '*' -> return AMult
        '/' -> return ADiv

-- A boolean expression is either of: true, false, boolean operator applied to two boolean expressions, relational operator
-- applied to two arithmetic expressions, parenthesized boolean expression
-- Again, since `and` has higher precedence than `or`, we further subdivide expressions into three categories
bexp :: Parsec String () BExp
bexp = spaces *> chainl1 conjunct orOp <* spaces
  where
    conjunct = spaces *> chainl1 unitExpr (try andOp) <* spaces
    unitExpr = try (between (char '(') (char ')') bexp) <|> try relExp <|> try true <|> false
    relExp = do
      spaces
      a1 <- aexp
      spaces
      symbol <- oneOf "><"
      spaces
      a2 <- aexp
      spaces
      case symbol of
        '>' -> return (BGt a1 a2)
        '<' -> return (BLt a1 a2)
    true = do
      string "true"
      return BTrue
    false = do
      string "false"
      return BFalse
    orOp = do
      spaces
      string "or"
      spaces
      return BOr
    andOp = do
      spaces
      string "and"
      spaces
      return BAnd

-- A statement is one of: assignment statement, sequence of statements, conditional statement, while loop
stmt :: Parsec String () Stmt
stmt = spaces *> chainl1 stmtUnit (try seqOp) <* spaces
  where
    stmtUnit = try assStmt <|> try condStmt <|> whileStmt
    assStmt = do
      x <- var
      spaces
      string ":="
      spaces
      a <- aexp
      return (SAss x a)
    condStmt = do
      string "if"
      spaces
      b <- bexp
      spaces
      string "then"
      spaces
      s1 <- between (char '{') (char '}') stmt
      spaces
      string "else"
      spaces
      s2 <- between (char '{') (char '}') stmt
      return (SIf b s1 s2)
    whileStmt = do
      string "while"
      spaces
      b <- bexp
      spaces
      string "do"
      spaces
      s' <- between (char '{') (char '}') stmt
      return (SWhile b s')
    seqOp = do
      spaces
      char ';'
      spaces
      return SSeq

{-
Tying everything together ...
-}
execute :: String -> Maybe St
execute code = do
  prog <- eitherToMaybe (parse While.stmt "" code)
  seval Map.empty prog
