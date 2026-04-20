module Main where

import Parser
import Test.Hspec 
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do  
        describe "Prelude.head" $ do
            -- Parser tests: Test file should be correctly parsed
            it "Parser: data types are correctly parsed" $ do
               contents <- readFile "resources/test.txt"
               case runParserEnd match' contents of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (prettyDType $ (\(x,_,_) -> head x) x) `shouldBe` "Unit\n  tt : Unit"

            it "Parser: pattern matrix is correctly parsed" $ do
               contents <- readFile "resources/test.txt"
               case runParserEnd match' contents of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (prettyPMat $ (\(_,x,_) -> x) x) `shouldBe` "(nat x zero y) | (nat zero zero nil) x\n(list (cons zero nil)) x\n(unit x) x"

            it "returns the first element of an *arbitrary* list" $
               property $ \x xs -> head (x:xs) == (x :: Int)

            it "throws an exception if used with an empty list" $ do
               evaluate (head []) `shouldThrow` anyException
