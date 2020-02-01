module While where

import Control.Monad
import qualified Data.Map as Map

-- Arithmetic expressions
data AExp =
  AId String |       -- Variables
  ANum Integer |     -- Integer Literals
  APlus AExp AExp |  -- Addition
  AMinus AExp AExp | -- Subtraction
  AMult AExp AExp |  -- Multiplication
  ADiv AExp AExp     -- Integer Divison

-- Boolean expressions
data BExp =
  BTrue |          -- true
  BFalse |         -- false
  BAnd BExp BExp | -- boolean AND
  BOr BExp BExp |  -- boolean OR
  BGt AExp AExp |  -- arithmetic greater than
  BLt AExp AExp    -- arithmetic less than

-- Statements
data Stmt =
  SAss String AExp |   -- Variable assignment
  SSeq Stmt Stmt |     -- Statement sequencing
  SIf BExp Stmt Stmt | -- Conditional statement
  SWhile BExp Stmt     -- While loop

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
