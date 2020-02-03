# While

Parser and evaluator for a subset of [While](https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/resources/while-language.pdf) in Haskell

## Usage

If you just want to see the interpreter in action, the only file you'll really need is `While.hs`. To avoid name clashes with other modules, you'll probably want to import it qualified. Furthermore, if you're not interested in the theory of the language or how the interpreter is implemented then you'll only need to import the `execute` function as follows:

```haskell
import qualified While (execute)
```

`While.execute` accepts a single string, the While program text, and returns the final state of the program provided that the execution was successful, wrapped in `Maybe`. The state of a program is simply a collection of associations between initialized variables and their values.

Example usage:

```haskell
module Main where

import Control.Monad
import qualified While (execute)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  code <- readFile "/path/to/your/while-program.txt"
  case While.execute code of
    Just finalSt -> do
      putStrLn "Execution successful"
      putStrLn "The final state is: {"
      forM_ (Map.foldrWithKey (\x n xs -> (x, n) : xs) [] finalSt) $ \(x, n) -> do
        putStrLn ("  " ++ x ++ ": " ++ show n ++ ";")
      putStrLn "}"
    Nothing -> putStrLn "The program failed to execute due to invalid syntax or attempting to reference uninitialized variables during runtime"
```

Two sample While programs, `prog1.txt` and `prog2.txt`, as well as their minified forms (`prog1.min.txt`, `prog2.min.txt`) have been provided to you for your perusal.

For those interested in the implementation details of the interpreter, please refer to section "`While.hs` - Synopsis". For those interested in the theory behind the While language, please refer to section "Syntax and Operational Semantics".

## Testing

To run the tests for `While.hs` (which tests each individual component as well as the interpreter itself), simply `cd` to your copy of this repo and run

```bash
$ ghc Main.hs && ./Main
```

You should see exactly 18 passed tests and no failures.

## `While.hs` - Synopsis

```haskell
{-
Abstract syntax of While
-}
-- Arithmetic expressions
data AExp
-- Boolean expressions
data BExp
-- Statements
data Stmt

{-
Operational semantics of While
-}
-- Program state
-- A program state is simply a collection of associations of initialized variables with their values
type St = Map String Integer
-- Evaluation of arithmetic expressions w.r.t. a given program state
-- The evaluation may fail if the expression contains references to uninitialized variables so the result is wrapped in Maybe
aeval :: St -> AExp -> Maybe Integer
-- Evaluation of boolean expressions w.r.t a given program state
beval :: St -> BExp -> Maybe Bool
-- Evaluation of statements w.r.t a given initial program state, yielding some final program state (if it does not enter an infinite loop)
seval :: St -> Stmt -> Maybe St

{-
Parsers for translating concrete syntax to abstract syntax (which can then be evaluated by our evaluators above)
-}
-- Parser for While variables
var :: Parsec String () String
-- Parser for arithmetic expressions
aexp :: Parsec String () AExp
-- Parser for boolean expressions
bexp :: Parsec String () BExp
-- Parser for statements
stmt :: Parsec String () Stmt

{-
The interpreter
Parses the provided program text, evaluates the resulting AST and returns the final state of the program if successful
-}
execute :: String -> Maybe St
```

## Syntax and Operational Semantics

Let us first define a few metavariables:

| Metavariable | Interpretation |
| --- | --- |
| `S` | statement |
| `a` | arithmetic expression |
| `x` | variable |
| `n` | integer literal |
| `b` | boolean expression |

The syntax of the subset of While implemented in this repo is then given as follows:

```
a ::= x
  |   n
  |   a + a
  |   a - a
  |   a * a
  |   a / a
  |   ( a )
b ::= true
  |   false
  |   a > a
  |   a < a
  |   b and b
  |   b or b
  |   ( b )
S ::= x := a
  |   S ; S
  |   if b then { S } else { S }
  |   while b do { S }
```

Note that `/` in While is (floored) integer division.

Now define a few notations:

| Notation | Interpretation |
| --- | --- |
| `x \|-> v // st` | Variable `x` maps to value `v` under program state `st` |
| `x \|-> v; st` | The updated program state that sends variable `x` to value `v` and all other variables to their corresponding values in the original state `st` |
| `e ==> v // st` | Arithmetic/boolean expression `e` evaluates to final value `v` under program state `st` |
| `st =[ S ]=> st'` | Starting with initial program state `st`, executing the statement `S` brings us to final program state `st'` |

Then our operational semantics can be expressed using inference rules as follows:

```
               x |-> n // st
               ------------- (A_Var)
               x ==> n // st

              a1 ==> n1 // st
              a2 ==> n2 // st
         ------------------------- (A_Plus)
         a1 + a2 ==> n1 + n2 // st

              a1 ==> n1 // st
              a2 ==> n2 // st
         ------------------------- (A_Minus)
         a1 - a2 ==> n1 - n2 // st

              a1 ==> n1 // st
              a2 ==> n2 // st
         ------------------------- (A_Mult)
         a1 * a2 ==> n1 * n2 // st

              a1 ==> n1 // st
              a2 ==> n2 // st
         ------------------------- (A_Div)
         a1 / a2 ==> n1 / n2 // st

             a1 ==> n1 // st
             a2 ==> n2 // st
                 n1 > n2
          ---------------------- (B_Gt_True)
          a1 > a2 ==> true // st

              a1 ==> n1 // st
              a2 ==> n2 // st
                 n1 <= n2
          ----------------------- (B_Gt_False)
          a1 > a2 ==> false // st

             a1 ==> n1 // st
             a2 ==> n2 // st
                 n1 < n2
          ---------------------- (B_Lt_True)
          a1 < a2 ==> true // st

              a1 ==> n1 // st
              a2 ==> n2 // st
                 n1 >= n2
          ----------------------- (B_Lt_False)
          a1 < a2 ==> false // st

             b1 ==> bv1 // st
             b2 ==> bv2 // st
      ------------------------------ (B_And)
      b1 and b2 ==> bv1 && bv2 // st

            b1 ==> bv1 // st
            b2 ==> bv2 // st
      ----------------------------- (B_Or)
      b1 or b2 ==> bv1 || bv2 // st

              a ==> n // st
      ------------------------------ (S_Ass)
      st =[ x := a ]=> (x |-> n; st)

             st =[ S1 ]=> st'
            st' =[ S2 ]=> st''
          ---------------------- (S_Seq)
          st =[ S1 ; S2 ]=> st''

             b ==> true // st
             st =[ S1 ]=> st'
------------------------------------------ (S_If_True)
st =[ if b then { S1 } else { S2 } ]=> st'

            b ==> false // st
             st =[ S2 ]=> st'
------------------------------------------ (S_If_False)
st =[ if b then { S1 } else { S2 } ]=> st'

            b ==> true // st
            st =[ S' ]=> st'
    st' =[ while b do { S' } ]=> st''
    --------------------------------- (S_While_True)
    st =[ while b do { S' } ]=> st''

           b ==> false // st
     ------------------------------ (S_While_False)
     st =[ while b do { S' } ]=> st
```

## Thanks

I probably would not have been able to learn how to use Parsec in Haskell to implement a full-blown parser for a toy imperative programming language in such a short period of time (less than 3 days!) were it not for [this amazing Parsec tutorial](https://www.cnblogs.com/ncore/p/6892500.html) which explains everything from the ground up (instead of jumping straight to a full-blown example as is typical of most Parsec tutorials out there). As for my evaluator and analysis of the syntax and operational semantics of While, they are heavily inspired by the first 2 volumes of [Software Foundations](https://softwarefoundations.cis.upenn.edu).

## LICENSE

[MIT](./LICENSE)
