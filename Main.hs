module Main where

import Test.Hspec
import Text.Parsec
import Data.Either
import qualified While
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "The While programming language" $ do
    -- AST of program in prog1.txt
    let prog1 = While.SSeq (While.SSeq (While.SSeq (While.SSeq (While.SSeq (While.SAss "fact" (While.ANum 1)) (While.SAss "val" (While.ANum 10000))) (While.SAss "cur" (While.AId "val"))) (While.SAss "mod" (While.ANum 1000000007))) (While.SWhile (While.BGt (While.AId "cur") (While.ANum 1)) (While.SSeq (While.SSeq (While.SAss "fact" (While.AMult (While.AId "fact") (While.AId "cur"))) (While.SAss "fact" (While.AMinus (While.AId "fact") (While.AMult (While.ADiv (While.AId "fact") (While.AId "mod")) (While.AId "mod"))))) (While.SAss "cur" (While.AMinus (While.AId "cur") (While.ANum 1)))))) (While.SAss "cur" (While.ANum 0))
    -- AST of program in prog2.txt
    let prog2 = While.SSeq (While.SSeq (While.SAss "a" (While.ANum 10)) (While.SAss "b" (While.ANum 100))) (While.SIf (While.BLt (While.AId "a") (While.AId "b")) (While.SSeq (While.SAss "min" (While.AId "a")) (While.SAss "max" (While.AId "b"))) (While.SSeq (While.SAss "min" (While.AId "b")) (While.SAss "max" (While.AId "a"))))
    describe "The statement evaluator" $ do
      it "should work for some simple examples" $ do
        Map.foldrWithKey (\x n xs -> (x, n) : xs) [] <$> (While.seval Map.empty prog1) `shouldBe`
          Just [("cur", 0), ("fact", 531950728), ("mod", 1000000007), ("val", 10000)]
        Map.foldrWithKey (\x n xs -> (x, n) : xs) [] <$> (While.seval Map.empty prog2) `shouldBe`
          Just [("a", 10), ("b", 100), ("max", 100), ("min", 10)]
    describe "The parser" $ do
      describe "For variables" $ do
        it "should accept variable names with 1-10 lowercase letters provided that they are not reserved keywords and reject otherwise" $ do
          parse While.var "" "cur" `shouldBe` Right (While.AId "cur")
          parse While.var "" "fact" `shouldBe` Right (While.AId "fact")
          parse While.var "" "mod" `shouldBe` Right (While.AId "mod")
          parse While.var "" "val" `shouldBe` Right (While.AId "val")
          parse While.var "" "a" `shouldBe` Right (While.AId "a")
          parse While.var "" "b" `shouldBe` Right (While.AId "b")
          parse While.var "" "max" `shouldBe` Right (While.AId "max")
          parse While.var "" "min" `shouldBe` Right (While.AId "min")
          parse While.var "" "hahahahaha" `shouldBe` Right (While.AId "hahahahaha")
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
      describe "For numerals" $ do
        it "should only accept integers in decimal form" $ do
          parse While.num "" "-1024" `shouldBe` Right (While.ANum (-1024))
          parse While.num "" "-512" `shouldBe` Right (While.ANum (-512))
          parse While.num "" "-256" `shouldBe` Right (While.ANum (-256))
          parse While.num "" "-128" `shouldBe` Right (While.ANum (-128))
          parse While.num "" "-64" `shouldBe` Right (While.ANum (-64))
          parse While.num "" "-32" `shouldBe` Right (While.ANum (-32))
          parse While.num "" "-16" `shouldBe` Right (While.ANum (-16))
          parse While.num "" "-8" `shouldBe` Right (While.ANum (-8))
          parse While.num "" "-4" `shouldBe` Right (While.ANum (-4))
          parse While.num "" "-2" `shouldBe` Right (While.ANum (-2))
          parse While.num "" "-1" `shouldBe` Right (While.ANum (-1))
          parse While.num "" "0" `shouldBe` Right (While.ANum 0)
          parse While.num "" "1" `shouldBe` Right (While.ANum 1)
          parse While.num "" "2" `shouldBe` Right (While.ANum 2)
          parse While.num "" "4" `shouldBe` Right (While.ANum 4)
          parse While.num "" "8" `shouldBe` Right (While.ANum 8)
          parse While.num "" "16" `shouldBe` Right (While.ANum 16)
          parse While.num "" "32" `shouldBe` Right (While.ANum 32)
          parse While.num "" "64" `shouldBe` Right (While.ANum 64)
          parse While.num "" "128" `shouldBe` Right (While.ANum 128)
          parse While.num "" "256" `shouldBe` Right (While.ANum 256)
          parse While.num "" "512" `shouldBe` Right (While.ANum 512)
          parse While.num "" "1024" `shouldBe` Right (While.ANum 1024)
          parse While.num "" "-0" `shouldSatisfy` isLeft
          parse While.num "" "haha" `shouldSatisfy` isLeft
          parse While.num "" "a1b2c3" `shouldSatisfy` isLeft
          parse While.num "" " 10 " `shouldSatisfy` isLeft
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
