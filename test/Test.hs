module Main where

import Parser
import Detection
import UsefulClause
import Test.Hspec 
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (intercalate)
import Data.Either (isLeft, isRight)


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
                  (_:_) -> "If this runs the code parsed" `shouldBe` "If this runs the code parsed"

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

            -- Parser semantic tests
            it "Parser semantic: each constructor returns the type it is supposed to positive" $ do
               let dts = [("Fruit", [("apple", ["Fruit"]), ("citrus", ["Citrus", "Fruit"])])]
               (dtypeConReturnsType dts) `shouldSatisfy` isRight

            it "Parser semantic: each constructor returns the type it is supposed to negative" $ do
               let dts = [("Fruit", [("apple", ["Fruit"]), ("citrus", ["Citrus"])])]
               (dtypeConReturnsType dts) `shouldSatisfy` isLeft

            -- Useful clause tests: Individual function tests
            it "Useful clause: getSigma works correctly on edge cases" $ do
               case runParserEnd pmat "(succ zero) nil\n\
                                    \zero (cons zero nil)\n\
                                    \two | (three | four) nil\n\
                                    \x nil" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (intercalate ", " (getSigma x)) `shouldBe` "succ, zero, two, three, four"

            it "Useful clause: specializedP (V) works correctly on edge cases" $ do
               case runParserEnd pmat "(cons one nil) nil\n\
                                    \(cons zero nil) nil\n\
                                    \nil | (nil | y) nil\n\
                                    \x nil" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (prettyPMat (specializedP "cons" 2 x)) `shouldBe` "one nil nil\nzero nil nil\ny y nil\nx x nil"

            it "Useful clause: defaultP works correctly on edge cases" $ do
               case runParserEnd pmat "(cons one nil) nil\n\
                                    \(cons zero nil) nil\n\
                                    \nil | (nil | y) nil\n\
                                    \x nil" of
                  []    -> error "\n\nParse failed.\n"
                  (x:_) -> (prettyPMat (defaultP x)) `shouldBe` "nil\nnil"

            it "Useful clause: Main function test negative example" $ do
               contents <- readFile "resources/test.txt"
               case runParserEnd match' contents of
                  []    -> error "\n\nParse failed.\n"
                  ((dts, p, _):_) -> ([(PCon "nat" [(PCon "zero" []), (PCon "zero" [])]), (PVar "x")] `isUsefulTo` p $ dts) `shouldBe` False

            it "Useful clause: Main function test positive example" $ do
               contents <- readFile "resources/test.txt"
               case runParserEnd match' contents of
                  []    -> error "\n\nParse failed.\n"
                  ((dts, p, _):_) -> ([(PCon "nat" [(PCon "zero" []), (PCon "x" []), (PVar "nil")]), (PVar "x")] `isUsefulTo` p $ dts) `shouldBe` True

            it "Useful clause: Bound variables same type positive example" $ do
               let p = [[(PVar "x"),  (PVar "x")],
                        [(PVar "x"),         (PCon "apple" [])],
                        [(PCon "orange" []), (PVar "x")]]
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])])]
               let v = [(PCon "apple" []), (PCon "orange" [])]
               (v `isUsefulTo` p $ dts) `shouldBe` True

            it "Useful clause: Bound variables same type negative example" $ do
               let p = [[(PVar "x"),  (PVar "y")],
                        [(PVar "x"),         (PCon "apple" [])],
                        [(PCon "orange" []), (PVar "x")]]
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])])]
               let v = [(PCon "apple" []), (PCon "orange" [])]
               (v `isUsefulTo` p $ dts) `shouldBe` False

            it "Useful clause: Bound variables nested positive example" $ do
               let p = [[(PCon "succ" [(PVar "y")]),  (PVar "y")],
                        [(PVar "x"),         (PCon "apple" [])],
                        [(PCon "zero" []), (PVar "x")]]
               let dts = [("Nat", [("zero", ["Nat"]), ("succ", ["Nat", "Nat"])]), ("Fruit", [("apple", ["Fruit"])])]
               let v = [(PCon "succ" [(PCon "zero" [])]), (PCon "zero" [])]
               (v `isUsefulTo` p $ dts) `shouldBe` True

            it "Useful clause: Bound variables nested negative example" $ do
               let p = [[(PCon "succ" [(PVar "y")]),  (PVar "y")],
                        [(PVar "x"),         (PCon "apple" [])],
                        [(PCon "zero" []), (PVar "x")]]
               let dts = [("Nat", [("zero", ["Nat"]), ("succ", ["Nat", "Nat"])]), ("Fruit", [("apple", ["Fruit"])])]
               let v = [(PCon "succ" [(PCon "zero" [])]), (PCon "orange" [])]
               (v `isUsefulTo` p $ dts) `shouldBe` False

            it "Detection: exhaustive function general test negative example" $ do
               contents <- readFile "resources/test.txt"
               case runParserEnd match' contents of
                  []    -> error "\n\nParse failed.\n"
                  ((dts, p, _):_) -> (p `isExhaustiveUnder` dts) `shouldBe` False

            it "Detection: exhaustive function general test positive example" $ do
               contents <- readFile "resources/test.txt"
               case runParserEnd match' contents of
                  []    -> error "\n\nParse failed.\n"
                  ((dts, p, _):_) -> (([(PVar "z"), (PVar "z")]:p) `isExhaustiveUnder` dts) `shouldBe` True

            it "Detection: exhaustive function test edge case not all constructors used of data type" $ do
               let p = [[(PCon "apple" [])],[(PCon "orange" [])]]
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"]), ("pear", ["Fruit"])])]
               ([(PVar "x")] `isUsefulTo` p $ dts) `shouldBe` True

            it "Detection: exhaustive function test edge case all constructors used of data type" $ do
               let p = [[(PCon "apple" [])],[(PCon "orange" [])],[(PCon "pear" [])]]
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"]), ("pear", ["Fruit"])])]
               ([(PVar "x")] `isUsefulTo` p $ dts) `shouldBe` False

            it "Detection: exhaustive function test edge case constructor exhaustion" $ do
               let p = [[(PCon "apple" []),  (PVar "x")],
                        [(PVar "x"),         (PCon "apple" [])],
                        [(PCon "orange" []), (PVar "x")]]
                     -- [(PVar "x"),         (PCon "orange" [])] This case isn't needed due to constuctor exhaustion
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])])]
               ([(PVar "x")] `isUsefulTo` p $ dts) `shouldBe` False

            it "Detection: contains useless row function positive example" $ do
               let p = [[(PCon "apple" []),  (PVar "x")],
                        [(PVar "x"),         (PCon "apple" [])],
                        [(PCon "orange" []), (PVar "x")],
                        [(PVar "x"),         (PCon "orange" [])]]
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])])]
               (containsUselessRow dts p) `shouldBe` (Just [(PVar "x"), (PCon "orange" [])])

            it "Detection: contains useless row function negative example" $ do
               let p = [[(PCon "apple" []),  (PVar "x")],
                        [(PVar "x"),         (PCon "apple" [])],
                        [(PCon "orange" []), (PVar "x")],
                        [(PVar "x"),         (PCon "orange" [])],
                        [(PVar "x"),         (PVar "y")]]
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"]), ("pear", ["Pear"])])]
               (containsUselessRow dts p) `shouldBe` (Nothing)

            it "Constructor multiple declaration" $ do
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])]), ("Vegetable", [("apple", ["Vegetable"])])]
               (dtypeConsUnique dts) `shouldSatisfy` isLeft

            it "Constructor multiple declaration" $ do
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])]), ("Vegetable", [("tomato", ["Vegetable"])])]
               (dtypeConsUnique dts) `shouldSatisfy` isRight
