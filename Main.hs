module Main where

import While
import Test.Hspec
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "The While statement evaluator" $ do
    it "should work for some simple examples" $ do
      {-
      fact := 1;
      val := 10000;
      cur := val;
      mod := 1000000007;
      while (cur > 1) do {
        fact := fact * cur;
        fact := fact - fact / mod * mod;
        cur := cur - 1
      };
      cur := 0
      -}
      let prog1 = SSeq (SSeq (SSeq (SSeq (SSeq (SAss "fact" (ANum 1)) (SAss "val" (ANum 10000))) (SAss "cur" (AId "val"))) (SAss "mod" (ANum 1000000007))) (SWhile (BGt (AId "cur") (ANum 1)) (SSeq (SSeq (SAss "fact" (AMult (AId "fact") (AId "cur"))) (SAss "fact" (AMinus (AId "fact") (AMult (ADiv (AId "fact") (AId "mod")) (AId "mod"))))) (SAss "cur" (AMinus (AId "cur") (ANum 1)))))) (SAss "cur" (ANum 0))
      Map.foldrWithKey (\x n xs -> (x, n) : xs) [] <$> (seval Map.empty prog1) `shouldBe`
        Just [("cur", 0), ("fact", 531950728), ("mod", 1000000007), ("val", 10000)]
      {-
      a := 10;
      b := 100;
      if (a < b) then {
        min := a;
        max := b
      } else {
        min := b;
        max := a
      }
      -}
      let prog2 = SSeq (SSeq (SAss "a" (ANum 10)) (SAss "b" (ANum 100))) (SIf (BLt (AId "a") (AId "b")) (SSeq (SAss "min" (AId "a")) (SAss "max" (AId "b"))) (SSeq (SAss "min" (AId "b")) (SAss "max" (AId "a"))))
      Map.foldrWithKey (\x n xs -> (x, n) : xs) [] <$> (seval Map.empty prog2) `shouldBe`
        Just [("a", 10), ("b", 100), ("max", 100), ("min", 10)]
