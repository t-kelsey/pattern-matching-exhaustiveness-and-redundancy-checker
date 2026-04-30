module Main where

import Parser
import Algo
import Test.Hspec 
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (intercalate)


main :: IO ()
main = hspec $ do  
        describe "Prelude.head" $ do

            -- Parser tests: Test file sections should be correctly parsed

            it "Parser: data types are correctly parsed" $ do
               case runParserEnd dtypes "data Unit where\n\
                                        \tt : Unit\n\
                                        \\n\
                                        \data Nat where\n\
                                        \zero : Nat\n\
                                        \suc  : Nat -> Nat" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (prettyDType $ head x) `shouldBe` "Unit\n  tt : Unit"

            it "Parser: pattern matrix is correctly parsed" $ do
               case runParserEnd pmat "(nat x zero y) | (nat zero zero nil)  x\n\
                                        \(list (cons zero nil))          x\n\
                                        \(unit x)                  x" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (prettyPMat x) `shouldBe` "(nat x zero y) | (nat zero zero nil) x\n(list (cons zero nil)) x\n(unit x) x"

            it "Parser: type to be matched is correctly parsed" $ do
               case runParserEnd vvec "OneOfThose Nat" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (prettyVVec x) `shouldBe` "OneOfThose Nat"

            it "Parser: test file is correctly loaded and parsed" $ do
               contents <- readFile "resources/test.txt"
               case runParserEnd match' contents of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> "If this runs the code parsed" `shouldBe` "If this runs the code parsed"

            -- Parser tests: Implicit and explicit data structures should be parsed correctly
            
            -- Con tests
            it "Parser: Constructor pattern with > 0 arity is handled correctly" $ do
               case runParserEnd p "(nat zero (succ zero) nil)" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon nat [(PCon zero []), (PCon succ [(PCon zero [])]), (PCon nil [])])"
            
            it "Parser: Constructor pattern with = 0 arity is handled correctly" $ do
               case runParserEnd p "(succ (succ zero))" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PCon zero [])])])"

            -- Var tests
            it "Parser: Variable pattern is correctly positively identified" $ do
               case runParserEnd p "(succ (succ x))" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PVar x)])])"

            it "Parser: Variable pattern is correctly negatively identified" $ do
               case runParserEnd p "(succ (succ X))" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PCon X [])])])"

            -- Or tests
            it "Parser: Or pattern is correctly identified" $ do
               case runParserEnd p "zero | one" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(POr (PCon zero []) | (PCon one []))"

            it "Parser: Or pattern implicit parenthesization is correct on rows" $ do
               case runParserEnd pvec "zero | one zero" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugPVec x) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"

            it "Parser: Or pattern explicit parenthesization is correct on rows" $ do
               case runParserEnd pvec "(zero | one) zero" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugPVec x) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"

            it "Parser: Or pattern is correctly identified in constructor pattern" $ do
               case runParserEnd p "(succ zero | one)" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(PCon succ [(POr (PCon zero []) | (PCon one []))])"
                  

            it "Parser: Or pattern is correct in nested or-patterns with mixed implicit and explicit parenthesization" $ do
               case runParserEnd p "zero | (one | two)" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (debugP x) `shouldBe` "(POr (PCon zero []) | (POr (PCon one []) | (PCon two [])))"

            -- Algorithm tests: Individual function tests
            it "Algorithm: getSigma works correctly on edge cases" $ do
               case runParserEnd pmat "(succ zero) nil\n\
                                    \(cons zero nil) nil\n\
                                    \two | (three | four) nil\n\
                                    \x nil" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (intercalate ", " (getSigma x)) `shouldBe` "succ, cons, two, three, four"