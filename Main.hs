module Main where

import Test.Hspec
import Text.Parsec
import Data.Either
import qualified While
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec $ do
  describe "The While programming language" $ do
    -- AST of program in prog1.txt
    let prog1 = While.SSeq (While.SSeq (While.SSeq (While.SSeq (While.SSeq (While.SAss "fact" (While.ANum 1)) (While.SAss "val" (While.ANum 10000))) (While.SAss "cur" (While.AId "val"))) (While.SAss "mod" (While.ANum 1000000007))) (While.SWhile (While.BGt (While.AId "cur") (While.ANum 1)) (While.SSeq (While.SSeq (While.SAss "fact" (While.AMult (While.AId "fact") (While.AId "cur"))) (While.SAss "fact" (While.AMinus (While.AId "fact") (While.AMult (While.ADiv (While.AId "fact") (While.AId "mod")) (While.AId "mod"))))) (While.SAss "cur" (While.AMinus (While.AId "cur") (While.ANum 1)))))) (While.SAss "cur" (While.ANum 0))
    -- AST of program in prog2.txt
    let prog2 = While.SSeq (While.SSeq (While.SAss "a" (While.ANum 10)) (While.SAss "b" (While.ANum 100))) (While.SIf (While.BLt (While.AId "a") (While.AId "b")) (While.SSeq (While.SAss "min" (While.AId "a")) (While.SAss "max" (While.AId "b"))) (While.SSeq (While.SAss "min" (While.AId "b")) (While.SAss "max" (While.AId "a"))))
    -- empty =[ prog1 ]=> st1
    let st1 = Map.fromList [("cur", 0), ("fact", 531950728), ("mod", 1000000007), ("val", 10000)]
    -- empty =[ prog2 ]=> st2
    let st2 = Map.fromList [("a", 10), ("b", 100), ("max", 100), ("min", 10)]
    describe "The statement evaluator" $ do
      it "should work for some simple examples" $ do
        While.seval Map.empty prog1 `shouldBe` Just st1
        While.seval Map.empty prog2 `shouldBe` Just st2
    describe "The parser" $ do
      describe "For variables" $ do
        it "should accept variable names with 1-10 lowercase letters provided that they are not reserved keywords and reject otherwise" $ do
          parse While.var "" "cur" `shouldBe` Right "cur"
          parse While.var "" "fact" `shouldBe` Right "fact"
          parse While.var "" "mod" `shouldBe` Right "mod"
          parse While.var "" "val" `shouldBe` Right "val"
          parse While.var "" "a" `shouldBe` Right "a"
          parse While.var "" "b" `shouldBe` Right "b"
          parse While.var "" "max" `shouldBe` Right "max"
          parse While.var "" "min" `shouldBe` Right "min"
          parse While.var "" "hahahahaha" `shouldBe` Right "hahahahaha"
          parse While.var "" "ahahahahaha" `shouldSatisfy` isLeft
          parse While.var "" "" `shouldSatisfy` isLeft
          parse While.var "" "and" `shouldSatisfy` isLeft
          parse While.var "" "or" `shouldSatisfy` isLeft
          parse While.var "" "true" `shouldSatisfy` isLeft
          parse While.var "" "false" `shouldSatisfy` isLeft
          parse While.var "" "if" `shouldSatisfy` isLeft
          parse While.var "" "then" `shouldSatisfy` isLeft
          parse While.var "" "else" `shouldSatisfy` isLeft
          parse While.var "" "while" `shouldSatisfy` isLeft
          parse While.var "" "do" `shouldSatisfy` isLeft
          parse While.var "" "5 rings" `shouldSatisfy` isLeft
          parse While.var "" "MyVar" `shouldSatisfy` isLeft
      describe "For arithmetic expressions" $ do
        it "should work for trivial examples involving only variables" $ do
          parse While.aexp "" "cur" `shouldBe` Right (While.AId "cur")
          parse While.aexp "" "fact" `shouldBe` Right (While.AId "fact")
          parse While.aexp "" "mod" `shouldBe` Right (While.AId "mod")
          parse While.aexp "" "val" `shouldBe` Right (While.AId "val")
          parse While.aexp "" "a" `shouldBe` Right (While.AId "a")
          parse While.aexp "" "b" `shouldBe` Right (While.AId "b")
          parse While.aexp "" "max" `shouldBe` Right (While.AId "max")
          parse While.aexp "" "min" `shouldBe` Right (While.AId "min")
          parse While.aexp "" "hahahahaha" `shouldBe` Right (While.AId "hahahahaha")
        it "should work for trivial examples involving only numerals" $ do
          parse While.aexp "" "-1024" `shouldBe` Right (While.ANum (-1024))
          parse While.aexp "" "-512" `shouldBe` Right (While.ANum (-512))
          parse While.aexp "" "-256" `shouldBe` Right (While.ANum (-256))
          parse While.aexp "" "-128" `shouldBe` Right (While.ANum (-128))
          parse While.aexp "" "-64" `shouldBe` Right (While.ANum (-64))
          parse While.aexp "" "-32" `shouldBe` Right (While.ANum (-32))
          parse While.aexp "" "-16" `shouldBe` Right (While.ANum (-16))
          parse While.aexp "" "-8" `shouldBe` Right (While.ANum (-8))
          parse While.aexp "" "-4" `shouldBe` Right (While.ANum (-4))
          parse While.aexp "" "-2" `shouldBe` Right (While.ANum (-2))
          parse While.aexp "" "-1" `shouldBe` Right (While.ANum (-1))
          parse While.aexp "" "0" `shouldBe` Right (While.ANum 0)
          parse While.aexp "" "1" `shouldBe` Right (While.ANum 1)
          parse While.aexp "" "2" `shouldBe` Right (While.ANum 2)
          parse While.aexp "" "4" `shouldBe` Right (While.ANum 4)
          parse While.aexp "" "8" `shouldBe` Right (While.ANum 8)
          parse While.aexp "" "16" `shouldBe` Right (While.ANum 16)
          parse While.aexp "" "32" `shouldBe` Right (While.ANum 32)
          parse While.aexp "" "64" `shouldBe` Right (While.ANum 64)
          parse While.aexp "" "128" `shouldBe` Right (While.ANum 128)
          parse While.aexp "" "256" `shouldBe` Right (While.ANum 256)
          parse While.aexp "" "512" `shouldBe` Right (While.ANum 512)
          parse While.aexp "" "1024" `shouldBe` Right (While.ANum 1024)
        it "should work for simple examples involving exactly one arithmetic operator" $ do
          parse While.aexp "" "3+5" `shouldBe` Right (While.APlus (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "3 +5" `shouldBe` Right (While.APlus (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "3+ 5" `shouldBe` Right (While.APlus (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "3 + 5" `shouldBe` Right (While.APlus (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "3          +   5" `shouldBe` Right (While.APlus (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "a + 5" `shouldBe` Right (While.APlus (While.AId "a") (While.ANum 5))
          parse While.aexp "" "3 + b" `shouldBe` Right (While.APlus (While.ANum 3) (While.AId "b"))
          parse While.aexp "" "a + b" `shouldBe` Right (While.APlus (While.AId "a") (While.AId "b"))
          parse While.aexp "" "5-3" `shouldBe` Right (While.AMinus (While.ANum 5) (While.ANum 3))
          parse While.aexp "" "5 -3" `shouldBe` Right (While.AMinus (While.ANum 5) (While.ANum 3))
          parse While.aexp "" "5- 3" `shouldBe` Right (While.AMinus (While.ANum 5) (While.ANum 3))
          parse While.aexp "" "5 - 3" `shouldBe` Right (While.AMinus (While.ANum 5) (While.ANum 3))
          parse While.aexp "" "5          -   3" `shouldBe` Right (While.AMinus (While.ANum 5) (While.ANum 3))
          parse While.aexp "" "b - 3" `shouldBe` Right (While.AMinus (While.AId "b") (While.ANum 3))
          parse While.aexp "" "5 - a" `shouldBe` Right (While.AMinus (While.ANum 5) (While.AId "a"))
          parse While.aexp "" "b - a" `shouldBe` Right (While.AMinus (While.AId "b") (While.AId "a"))
          parse While.aexp "" "3*5" `shouldBe` Right (While.AMult (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "3 *5" `shouldBe` Right (While.AMult (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "3* 5" `shouldBe` Right (While.AMult (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "3 * 5" `shouldBe` Right (While.AMult (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "3   *          5" `shouldBe` Right (While.AMult (While.ANum 3) (While.ANum 5))
          parse While.aexp "" "a * 5" `shouldBe` Right (While.AMult (While.AId "a") (While.ANum 5))
          parse While.aexp "" "3 * b" `shouldBe` Right (While.AMult (While.ANum 3) (While.AId "b"))
          parse While.aexp "" "a * b" `shouldBe` Right (While.AMult (While.AId "a") (While.AId "b"))
          parse While.aexp "" "15/5" `shouldBe` Right (While.ADiv (While.ANum 15) (While.ANum 5))
          parse While.aexp "" "15 /5" `shouldBe` Right (While.ADiv (While.ANum 15) (While.ANum 5))
          parse While.aexp "" "15/ 5" `shouldBe` Right (While.ADiv (While.ANum 15) (While.ANum 5))
          parse While.aexp "" "15 / 5" `shouldBe` Right (While.ADiv (While.ANum 15) (While.ANum 5))
          parse While.aexp "" "15   /          5" `shouldBe` Right (While.ADiv (While.ANum 15) (While.ANum 5))
          parse While.aexp "" "b / 5" `shouldBe` Right (While.ADiv (While.AId "b") (While.ANum 5))
          parse While.aexp "" "15 / a" `shouldBe` Right (While.ADiv (While.ANum 15) (While.AId "a"))
          parse While.aexp "" "b / a" `shouldBe` Right (While.ADiv (While.AId "b") (While.AId "a"))
        it "should treat operators as left-associative" $ do
          parse While.aexp "" "1 - a - -3 + 4 + b" `shouldBe`
            Right (While.APlus (While.APlus (While.AMinus (While.AMinus (While.ANum 1) (While.AId "a")) (While.ANum (-3))) (While.ANum 4)) (While.AId "b"))
          parse While.aexp "" "108 / 9 * fuel * -53 / 217" `shouldBe`
            Right (While.ADiv (While.AMult (While.AMult (While.ADiv (While.ANum 108) (While.ANum 9)) (While.AId "fuel")) (While.ANum (-53))) (While.ANum 217))
        it "should respect the order of operations" $ do
          parse While.aexp "" "3 + 9 * 2 - 7 / 3 * 10 + 1" `shouldBe`
            Right (While.APlus (While.AMinus (While.APlus (While.ANum 3) (While.AMult (While.ANum 9) (While.ANum 2))) (While.AMult (While.ADiv (While.ANum 7) (While.ANum 3)) (While.ANum 10))) (While.ANum 1))
        it "should work for simple parenthesized expressions" $ do
          parse While.aexp "" "(3 + a) * 2 - -1 * (-3 - -5) / b" `shouldBe`
            Right (While.AMinus (While.AMult (While.APlus (While.ANum 3) (While.AId "a")) (While.ANum 2)) (While.ADiv (While.AMult (While.ANum (-1)) (While.AMinus (While.ANum (-3)) (While.ANum (-5)))) (While.AId "b")))
        it "should work for nested parenthesized expressions" $ do
          parse While.aexp "" "e + 5 * (((3 + a) * 2 - -1 * (-3 - -5) / b) / d)" `shouldBe`
            Right (While.APlus (While.AId "e") (While.AMult (While.ANum 5) (While.ADiv (While.AMinus (While.AMult (While.APlus (While.ANum 3) (While.AId "a")) (While.ANum 2)) (While.ADiv (While.AMult (While.ANum (-1)) (While.AMinus (While.ANum (-3)) (While.ANum (-5)))) (While.AId "b"))) (While.AId "d"))))
          parse While.aexp "" "e + 5 * ( ( ( 3 + a ) * 2 - -1 * ( -3 - -5 ) / b ) / d )" `shouldBe`
            Right (While.APlus (While.AId "e") (While.AMult (While.ANum 5) (While.ADiv (While.AMinus (While.AMult (While.APlus (While.ANum 3) (While.AId "a")) (While.ANum 2)) (While.ADiv (While.AMult (While.ANum (-1)) (While.AMinus (While.ANum (-3)) (While.ANum (-5)))) (While.AId "b"))) (While.AId "d"))))
      describe "For boolean expressions" $ do
        it "should successfully parse boolean literals" $ do
          parse While.bexp "" "true" `shouldBe` Right While.BTrue
          parse While.bexp "" "false" `shouldBe` Right While.BFalse
        it "should successfully parse simple boolean expressions with only 1 relational operator" $ do
          parse While.bexp "" "a + b / 5 > c * d - 7" `shouldBe`
            Right (While.BGt (While.APlus (While.AId "a") (While.ADiv (While.AId "b") (While.ANum 5))) (While.AMinus (While.AMult (While.AId "c") (While.AId "d")) (While.ANum 7)))
          parse While.bexp "" "a + b / 5 < c * d - 7" `shouldBe`
            Right (While.BLt (While.APlus (While.AId "a") (While.ADiv (While.AId "b") (While.ANum 5))) (While.AMinus (While.AMult (While.AId "c") (While.AId "d")) (While.ANum 7)))
        it "should successfully parse simple boolean expressions with exactly 1 boolean operator" $ do
          parse While.bexp "" "(3 + 4) * 2 < 13 and 1 + (2 + 3) + 4 > 6" `shouldBe`
            Right (While.BAnd (While.BLt (While.AMult (While.APlus (While.ANum 3) (While.ANum 4)) (While.ANum 2)) (While.ANum 13)) (While.BGt (While.APlus (While.APlus (While.ANum 1) (While.APlus (While.ANum 2) (While.ANum 3))) (While.ANum 4)) (While.ANum 6)))
          parse While.bexp "" "(3 + 4) * 2 < 13 or 1 + (2 + 3) + 4 > 6" `shouldBe`
            Right (While.BOr (While.BLt (While.AMult (While.APlus (While.ANum 3) (While.ANum 4)) (While.ANum 2)) (While.ANum 13)) (While.BGt (While.APlus (While.APlus (While.ANum 1) (While.APlus (While.ANum 2) (While.ANum 3))) (While.ANum 4)) (While.ANum 6)))
        it "should treat boolean operators as left-associative" $ do
          parse While.bexp "" "true and false and false" `shouldBe` Right (While.BAnd (While.BAnd While.BTrue While.BFalse) While.BFalse)
          parse While.bexp "" "false or true or false" `shouldBe` Right (While.BOr (While.BOr While.BFalse While.BTrue) While.BFalse)
        it "should give `and` a higher precedence than `or`" $ do
          parse While.bexp "" "a > b and c < d or false" `shouldBe` Right (While.BOr (While.BAnd (While.BGt (While.AId "a") (While.AId "b")) (While.BLt (While.AId "c") (While.AId "d"))) While.BFalse)
          parse While.bexp "" "a < b or true and c > d" `shouldBe` Right (While.BOr (While.BLt (While.AId "a") (While.AId "b")) (While.BAnd While.BTrue (While.BGt (While.AId "c") (While.AId "d"))))
        it "should work for simple parenthesized expressions" $ do
          parse While.bexp "" "true and (false or true)" `shouldBe` Right (While.BAnd While.BTrue (While.BOr While.BFalse While.BTrue))
          parse While.bexp "" "(false or true) and true" `shouldBe` Right (While.BAnd (While.BOr While.BFalse While.BTrue) While.BTrue)
        it "should work for nested parenthesized expressions" $ do
          parse While.bexp "" "((a + b) * c < d / (e - f) or g + (h - i) > j) and ((true and (k < l or m > n)) and (false or true))" `shouldBe`
            Right (While.BAnd (While.BOr (While.BLt (While.AMult (While.APlus (While.AId "a") (While.AId "b")) (While.AId "c")) (While.ADiv (While.AId "d") (While.AMinus (While.AId "e") (While.AId "f")))) (While.BGt (While.APlus (While.AId "g") (While.AMinus (While.AId "h") (While.AId "i"))) (While.AId "j"))) (While.BAnd (While.BAnd While.BTrue (While.BOr (While.BLt (While.AId "k") (While.AId "l")) (While.BGt (While.AId "m") (While.AId "n")))) (While.BOr While.BFalse While.BTrue)))
          parse While.bexp "" "( ( a + b ) * c < d / ( e - f ) or g + ( h - i ) > j ) and ( ( true and ( k < l or m > n ) ) and ( false or true ) )" `shouldBe`
            Right (While.BAnd (While.BOr (While.BLt (While.AMult (While.APlus (While.AId "a") (While.AId "b")) (While.AId "c")) (While.ADiv (While.AId "d") (While.AMinus (While.AId "e") (While.AId "f")))) (While.BGt (While.APlus (While.AId "g") (While.AMinus (While.AId "h") (While.AId "i"))) (While.AId "j"))) (While.BAnd (While.BAnd While.BTrue (While.BOr (While.BLt (While.AId "k") (While.AId "l")) (While.BGt (While.AId "m") (While.AId "n")))) (While.BOr While.BFalse While.BTrue)))
      describe "For statements" $ do
        it "should work for some simple examples" $ do
          prog1Str <- readFile "./prog1.txt"
          parse While.stmt "" prog1Str `shouldBe` Right prog1
          prog2Str <- readFile "./prog2.txt"
          parse While.stmt "" prog2Str `shouldBe` Right prog2
          prog1MinStr <- readFile "./prog1.min.txt"
          parse While.stmt "" prog1MinStr `shouldBe` Right prog1
          prog2MinStr <- readFile "./prog2.min.txt"
          parse While.stmt "" prog2MinStr `shouldBe` Right prog2
    describe "The interpreter" $ do
      it "should work for some simple examples" $ do
        prog1Str <- readFile "./prog1.txt"
        While.execute prog1Str `shouldBe` Just st1
        prog2Str <- readFile "./prog2.txt"
        While.execute prog2Str `shouldBe` Just st2
        prog1MinStr <- readFile "./prog1.min.txt"
        While.execute prog1MinStr `shouldBe` Just st1
        prog2MinStr <- readFile "./prog2.min.txt"
        While.execute prog2MinStr `shouldBe` Just st2
        While.execute "not a program" `shouldBe` Nothing
        While.execute "x := a" `shouldBe` Nothing
        While.execute "a := 42; b := 13; syntax error!" `shouldBe` Nothing
        While.execute "a := 10; while (a > 0) do { a := a - 1 }; a; b := 1" `shouldBe` Nothing
        While.execute "if (0 > 3) then { answer := 13 } else { answer := 42 }; answer := true; answer := 100" `shouldBe` Nothing
