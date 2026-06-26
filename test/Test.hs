module Main where

import Parser
import Detection
import UsefulClause
import Test.Hspec 
import Data.List (intercalate)
import Data.Either (isLeft, isRight)

main :: IO ()
main = hspec $ do  
        describe "Prelude.head" $ do

            -- Parser tests: Test file sections should be correctly parsed

            -- If the datatypes and pattern matrix parse, run the warnings function on them
            it "Parser: data types are correctly parsed" $ do
               contents <- readFile "resources/input.txt"
               ((dts, pm):_) <- runParserEnd match' contents

               (prettyDType $ head contents) `shouldBe` "Unit\n  tt : Unit"

            it "Parser: pattern matrix is correctly parsed" $ do
               x <- fixturePMat "test/integration/parser_pattern_matrix_or_constructor_rows.txt"
               (prettyPMat x) `shouldBe` "((nat x zero y) | (nat zero zero nil)) x\n(list (cons zero nil)) x\n(unit x) x"

            it "Parser: upper-case constructor pattern row is correctly parsed" $ do
               x <- fixturePVec "test/integration/parser_pattern_row_uppercase_constructors.txt"
               (debugPVec x) `shouldBe` "[(PCon OneOfThose []), (PCon Nat [])]"

            it "Parser: test file is correctly loaded and parsed" $ do
               _ <- parseMatchFixture "test/integration/parser_full_match_unit_nat_list_one_of_those.txt"
               "If this runs the code parsed" `shouldBe` "If this runs the code parsed"

            -- Parser tests: Implicit and explicit data structures should be parsed correctly
            
            -- Con tests
            it "Parser: Constructor pattern with > 0 arity is handled correctly" $ do
               x <- fixturePattern "test/integration/parser_constructor_pattern_nat_with_three_arguments.txt"
               (debugP x) `shouldBe` "(PCon nat [(PCon zero []), (PCon succ [(PCon zero [])]), (PCon nil [])])"
            
            it "Parser: Constructor pattern with = 0 arity is handled correctly" $ do
               x <- fixturePattern "test/integration/parser_constructor_pattern_nested_successor.txt"
               (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PCon zero [])])])"

            -- Var tests
            it "Parser: Variable pattern is correctly positively identified" $ do
               x <- fixturePattern "test/integration/parser_variable_pattern_lowercase_x.txt"
               (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PVar x)])])"

            it "Parser: Variable pattern is correctly negatively identified" $ do
               x <- fixturePattern "test/integration/parser_constructor_pattern_uppercase_x.txt"
               (debugP x) `shouldBe` "(PCon succ [(PCon succ [(PCon X [])])])"

            -- Or tests
            it "Parser: Or pattern is correctly identified" $ do
               x <- fixturePattern "test/integration/parser_or_pattern_zero_or_one.txt"
               (debugP x) `shouldBe` "(POr (PCon zero []) | (PCon one []))"

            it "Parser: Or pattern implicit parenthesization is correct on rows" $ do
               x <- fixturePVec "test/integration/parser_or_pattern_implicit_row_parentheses.txt"
               (debugPVec x) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"

            it "Parser: Or pattern explicit parenthesization is correct on rows" $ do
               x <- fixturePVec "test/integration/parser_or_pattern_explicit_row_parentheses.txt"
               (debugPVec x) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"

            it "Parser: Or pattern is correctly identified in constructor pattern" $ do
               x <- fixturePattern "test/integration/parser_or_pattern_inside_successor_constructor.txt"
               (debugP x) `shouldBe` "(PCon succ [(POr (PCon zero []) | (PCon one []))])"
                  

            it "Parser: Or pattern is correct in nested or-patterns with mixed implicit and explicit parenthesization" $ do
               x <- fixturePattern "test/integration/parser_nested_or_pattern_zero_one_two.txt"
               (debugP x) `shouldBe` "(POr (PCon zero []) | (POr (PCon one []) | (PCon two [])))"

            -- Parser semantic tests
            it "Parser semantic: each constructor returns the type it is supposed to positive" $ do
               let dts = [("Fruit", [("apple", ["Fruit"]), ("citrus", ["Citrus", "Fruit"])])]
               (dtypeConReturnsType dts) `shouldSatisfy` isRight

            it "Parser semantic: each constructor returns the type it is supposed to negative" $ do
               let dts = [("Fruit", [("apple", ["Fruit"]), ("citrus", ["Citrus"])])]
               (dtypeConReturnsType dts) `shouldSatisfy` isLeft

            -- Useful clause tests: Individual function tests
            it "Useful clause: getSigma works correctly on edge cases" $ do
               x <- fixturePMat "test/integration/useful_get_sigma_collects_constructor_names_from_or_patterns.txt"
               (intercalate ", " (getSigma x)) `shouldBe` "succ, zero, two, three, four"

            it "Useful clause: specializedP (V) works correctly on edge cases" $ do
               x <- fixturePMat "test/integration/useful_specialized_p_expands_cons_constructor_rows.txt"
               (prettyPMat (specializedP "cons" 2 x)) `shouldBe` "one nil nil\nzero nil nil\ny y nil\nx x nil"

            it "Useful clause: defaultP works correctly on edge cases" $ do
               x <- fixturePMat "test/integration/useful_default_p_keeps_variable_and_nonmatching_rows.txt"
               (prettyPMat (defaultP x)) `shouldBe` "nil\nnil"

            it "Useful clause: Main function test negative example" $ do
               (dts, p) <- parseMatchFixture "test/integration/useful_clause_nat_zero_zero_is_not_useful.txt"
               ([(PCon "nat" [(PCon "zero" []), (PCon "zero" [])]), (PVar "x")] `isUsefulTo` p $ dts) `shouldBe` False

            it "Useful clause: Main function test positive example" $ do
               (dts, p) <- parseMatchFixture "test/integration/useful_clause_nat_zero_x_nil_is_useful.txt"
               ([(PCon "nat" [(PCon "zero" []), (PCon "x" []), (PVar "nil")]), (PVar "x")] `isUsefulTo` p $ dts) `shouldBe` True

            it "Detection: exhaustive function general test negative example" $ do
               (dts, p) <- parseMatchFixture "test/integration/detection_exhaustiveness_one_of_those_matrix_is_not_exhaustive.txt"
               (p `isExhaustiveUnder` dts) `shouldBe` False

            it "Detection: exhaustive function general test positive example" $ do
               (dts, p) <- parseMatchFixture "test/integration/detection_exhaustiveness_one_of_those_matrix_with_wildcard_is_exhaustive.txt"
               (([(PVar "z"), (PVar "z")]:p) `isExhaustiveUnder` dts) `shouldBe` True

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

            it "Type checking: Constructor multiple declaration" $ do
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])]), ("Vegetable", [("apple", ["Vegetable"])])]
               (dtypeNamesUnique dts) `shouldSatisfy` isLeft

            it "Type checking: Constructor multiple declaration" $ do
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])]), ("Vegetable", [("tomato", ["Vegetable"])])]
               (dtypeNamesUnique dts) `shouldSatisfy` isRight

            it "Type checking: Bound variables multiple declaration" $ do
               let p = [[(PVar "x"),  (POr (PVar "y") (PVar "x"))]]
               let dts = [("Fruit", [("apple", ["Fruit"]), ("orange", ["Fruit"])])]
               let v = [(PCon "apple" []), (PCon "orange" [])]
               (pmatVarsUnique p) `shouldSatisfy` isLeft

            it "Type checking: Bound variables multiple declaration positive" $ do
               let p = [[(PCon "succ" [(PVar "y")]),  (PVar "x")],
                        [(PVar "x"),         (PCon "apple" [])],
                        [(PCon "zero" []), (PVar "x")]]
               let dts = [("Nat", [("zero", ["Nat"]), ("succ", ["Nat", "Nat"])]), ("Fruit", [("apple", ["Fruit"])])]
               let v = [(PCon "succ" [(PCon "zero" [])]), (PCon "zero" [])]
               (pmatVarsUnique p) `shouldSatisfy` isRight
