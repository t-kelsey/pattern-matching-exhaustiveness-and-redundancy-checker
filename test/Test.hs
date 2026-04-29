module Main where

import Parser
import Test.Hspec 
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do  
        describe "Prelude.head" $ do
            -- Parser tests: Test file sections should be correctly parsed
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

            it "Parser: type to be matched is correctly parsed" $ do
               contents <- readFile "resources/test.txt"
               case runParserEnd match' contents of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (prettyType $ (\(_,_,x) -> x) x) `shouldBe` "OneOfThose Nat"

            -- Parser tests: Implicit and explicit data structures should be parsed correctly
            it "Parser: Constructor pattern with > 0 arity is handled correctly" $ do
               case runParserEnd p "(nat zero (succ zero) nil)" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon nat [(PCon zero []), (PCon succ [(PCon zero [])]), (PCon nil [])])"
            
            it "Parser: Constructor pattern with = 0 arity is handled correctly" $ do
               case runParserEnd p "(succ (succ zero))" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PCon zero [])])])"

            it "Parser: Variable pattern is correctly positively identified" $ do
               case runParserEnd p "(succ (succ x))" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PVar x)])])"

            it "Parser: Variable pattern is correctly negatively identified" $ do
               case runParserEnd p "(succ (succ X))" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PCon X [])])])"

            it "Parser: Or pattern is correctly identified" $ do
               case runParserEnd p "zero | one" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(POr (PCon zero []) | (PCon one []))"

            it "Parser: Or pattern implicit parenthesization is correct on rows" $ do
               case runParserEnd pvec "zero | one zero" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugPVec x) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"

            it "Parser: Or pattern is correctly identified in constructor pattern" $ do
               case runParserEnd p "(succ zero | one)" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon succ [(POr (PCon zero []) | (PCon one []))])"