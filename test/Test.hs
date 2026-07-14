module Main where

import Parser
import Detection
import UsefulClause
import Test.Hspec
import Data.List (intercalate)
import Data.Either (isLeft, isRight)

-- The main test suite for testing all the individual functions as integration tests.
main :: IO ()
main = hspec $ do
        describe "Prelude.head" $ do

            -- Parser tests: Test file sections should be correctly parsed

            -- If the datatypes and pattern matrix parse, run the warnings function on them
            it "Parser: data types are correctly parsed" $ do
               contents <- readFile "test/integration/parsing/parser_data_types.txt"
               case runParserEnd match' contents of

                  ((dts, _):_) -> (prettyDType $ dtsHead dts) `shouldBe` "Unit\n  tt : Unit"
                  _            -> error ""

            it "Parser: pattern matrix is correctly parsed" $ do
               contents <- readFile "test/integration/parsing/parser_pattern_matrix_or_constructor_rows.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (prettyPMat pm) `shouldBe` "((nat x zero y) | (nat zero zero nil)) x\n(list (cons zero nil)) x\n(unit x) x"
                  _            -> error ""

            it "Parser: upper-case constructor pattern row is correctly parsed" $ do
               contents <- readFile "test/integration/parsing/parser_pattern_row_uppercase_constructors.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugPVec $ pMatHead pm) `shouldBe` "[(PCon OneOfThose []), (PCon Nat [])]"
                  _            -> error ""

            it "Parser: test file is correctly loaded and parsed" $ do
               contents <- readFile "test/integration/parsing/parser_full_match_unit_nat_list_one_of_those.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (length dts, length pm) `shouldBe` (4, 3)
                  _            -> error ""

            -- Parser tests: Implicit and explicit data structures should be parsed correctly

            -- Con tests
            it "Parser: Constructor pattern with > 0 arity is handled correctly" $ do
               contents <- readFile "test/integration/parsing/parser_constructor_pattern_nat_with_three_arguments.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugP $ pVecHead $ pMatHead pm) `shouldBe` "(PCon nat [(PCon zero []), (PCon succ [(PCon zero [])]), (PCon nil [])])"
                  _            -> error ""

            it "Parser: Constructor pattern with = 0 arity is handled correctly" $ do
               contents <- readFile "test/integration/parsing/parser_constructor_pattern_nested_successor.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugP $ pVecHead $ pMatHead pm) `shouldBe` "(PCon succ [(PCon succ [(PCon zero [])])])"
                  _            -> error ""

            -- Var tests
            it "Parser: Variable pattern is correctly positively identified" $ do
               contents <- readFile "test/integration/parsing/parser_variable_pattern_lowercase_x.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugP $ pVecHead $ pMatHead pm) `shouldBe` "(PCon succ [(PCon succ [(PVar x)])])"
                  _            -> error ""

            it "Parser: Variable pattern is correctly negatively identified" $ do
               contents <- readFile "test/integration/parsing/parser_constructor_pattern_uppercase_x.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugP $ pVecHead $ pMatHead pm) `shouldBe` "(PCon succ [(PCon succ [(PCon X [])])])"
                  _            -> error ""

            -- Or tests
            it "Parser: Or pattern is correctly identified" $ do
               contents <- readFile "test/integration/parsing/parser_or_pattern_zero_or_one.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugP $ pVecHead $ pMatHead pm) `shouldBe` "(POr (PCon zero []) | (PCon one []))"
                  _            -> error ""

            it "Parser: Or pattern implicit parenthesization is correct on rows" $ do
               contents <- readFile "test/integration/parsing/parser_or_pattern_implicit_row_parentheses.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugPVec $ pMatHead pm) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"
                  _            -> error ""

            it "Parser: Or pattern explicit parenthesization is correct on rows" $ do
               contents <- readFile "test/integration/parsing/parser_or_pattern_explicit_row_parentheses.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugPVec $ pMatHead pm) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"
                  _            -> error ""

            it "Parser: Or pattern is correctly identified in constructor pattern" $ do
               contents <- readFile "test/integration/parsing/parser_or_pattern_inside_successor_constructor.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugP $ pVecHead $ pMatHead pm) `shouldBe` "(PCon succ [(POr (PCon zero []) | (PCon one []))])"
                  _            -> error ""

            it "Parser: Or pattern is correct in nested or-patterns with mixed implicit and explicit parenthesization" $ do
               contents <- readFile "test/integration/parsing/parser_nested_or_pattern_zero_one_two.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (debugP $ pVecHead $ pMatHead pm) `shouldBe` "(POr (PCon zero []) | (POr (PCon one []) | (PCon two [])))"
                  _            -> error ""

            -- Parser semantic tests
            it "Parser semantic: each constructor returns the type it is supposed to positive" $ do
               contents <- readFile "test/integration/parsing/parser_semantic_constructor_returns_type_positive.txt"
               case runParserEnd match' contents of

                  ((dts, _):_) -> (dtypeConReturnsType dts) `shouldSatisfy` isRight
                  _            -> error ""

            it "Parser semantic: each constructor returns the type it is supposed to negative" $ do
               contents <- readFile "test/integration/parsing/parser_semantic_constructor_returns_type_negative.txt"
               case runParserEnd match' contents of

                  ((dts, _):_) -> (dtypeConReturnsType dts) `shouldSatisfy` isLeft
                  _            -> error ""

            -- Useful clause tests: Individual function tests
            it "Useful clause: getSigma works correctly on edge cases" $ do
               contents <- readFile "test/integration/algorithm/useful_get_sigma_collects_constructor_names_from_or_patterns.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (intercalate ", " (getSigma pm)) `shouldBe` "succ, zero, two, three, four"
                  _            -> error ""

            it "Useful clause: specializedP (V) works correctly on edge cases" $ do
               contents <- readFile "test/integration/algorithm/useful_specialized_p_expands_cons_constructor_rows.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (prettyPMat (specializedP "cons" 2 pm)) `shouldBe` "one nil nil\nzero nil nil\ny y nil\nx x nil"
                  _            -> error ""

            it "Useful clause: defaultP works correctly on edge cases" $ do
               contents <- readFile "test/integration/algorithm/useful_default_p_keeps_variable_and_nonmatching_rows.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (prettyPMat (defaultP pm)) `shouldBe` "nil\nnil"
                  _            -> error ""

            it "Useful clause: Main function test negative example" $ do
               contents <- readFile "test/integration/algorithm/useful_clause_nat_zero_zero_is_not_useful.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pMatHead pm `isUsefulTo` pMatTail pm $ dts) `shouldBe` False
                  _            -> error ""

            it "Useful clause: Main function test positive example" $ do
               contents <- readFile "test/integration/algorithm/useful_clause_nat_zero_x_nil_is_useful.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pMatHead pm `isUsefulTo` pMatTail pm $ dts) `shouldBe` True
                  _            -> error ""

            it "Detection: exhaustive function general test negative example" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_one_of_those_matrix_is_not_exhaustive.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pm `isExhaustiveUnder` dts) `shouldBe` False
                  _            -> error ""

            it "Detection: exhaustive function general test positive example" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_one_of_those_matrix_with_wildcard_is_exhaustive.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pm `isExhaustiveUnder` dts) `shouldBe` True
                  _            -> error ""

            it "Detection: exhaustive function test edge case not all constructors used of data type" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_fruit_missing_constructor_is_useful.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pMatHead pm `isUsefulTo` pMatTail pm $ dts) `shouldBe` True
                  _            -> error ""

            it "Detection: exhaustive function test edge case all constructors used of data type" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_fruit_all_constructors_used_is_not_useful.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pMatHead pm `isUsefulTo` pMatTail pm $ dts) `shouldBe` False
                  _            -> error ""

            it "Detection: exhaustive function test edge case constructor exhaustion" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_fruit_constructor_exhaustion.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pMatHead pm `isUsefulTo` pMatTail pm $ dts) `shouldBe` False
                  _            -> error ""

            it "Detection: contains useless row function positive example" $ do
               contents <- readFile "test/integration/algorithm/detection_contains_useless_row_positive.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (containsUselessRow dts pm) `shouldBe` (Just $ last pm)
                  _            -> error ""

            it "Detection: contains useless row function negative example" $ do
               contents <- readFile "test/integration/algorithm/detection_contains_useless_row_negative.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (containsUselessRow dts pm) `shouldBe` Nothing
                  _            -> error ""

            it "Witness: cases are exhaustive" $ do
               contents <- readFile "test/integration/algorithm/witness_cases_exhaustive.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (witness dts pm (length $ pMatHead pm)) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Witness: multiple constructors missing" $ do
               contents <- readFile "test/integration/algorithm/witness_multiple_constructors_missing.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (witness dts pm (length $ pMatHead pm)) `shouldBe` (Right $ [(PCon "one" []), (PVar "_")])
                  _            -> error ""

            it "Type checking: Constructor multiple declaration" $ do
               contents <- readFile "test/integration/type_checking/type_checking_constructor_multiple_declaration_negative.txt"
               case runParserEnd match' contents of

                  ((dts, _):_) -> (dtypeNamesUnique dts) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Type checking: Constructor multiple declaration" $ do
               contents <- readFile "test/integration/type_checking/type_checking_constructor_multiple_declaration_positive.txt"
               case runParserEnd match' contents of

                  ((dts, _):_) -> (dtypeNamesUnique dts) `shouldSatisfy` isRight
                  _            -> error ""

            it "Type checking: Referenced type is not defined" $ do
               contents <- readFile "test/integration/type_checking/type_checking_dtype_types_exist_negative.txt"
               case runParserEnd match' contents of

                  ((dts, _):_) -> (dtypeTypesExist dts) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Type checking: Referenced types are defined" $ do
               contents <- readFile "test/integration/type_checking/type_checking_dtype_types_exist_positive.txt"
               case runParserEnd match' contents of

                  ((dts, _):_) -> (dtypeTypesExist dts) `shouldSatisfy` isRight
                  _            -> error ""

            it "Type checking: Pattern constructor is not defined" $ do
               contents <- readFile "test/integration/type_checking/type_checking_pattern_constructor_exists_negative.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pmatConsExist dts pm) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Type checking: Pattern constructors are defined" $ do
               contents <- readFile "test/integration/type_checking/type_checking_pattern_constructor_exists_positive.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pmatConsExist dts pm) `shouldSatisfy` isRight
                  _            -> error ""

            it "Type checking: Pattern matrix row has wrong size" $ do
               contents <- readFile "test/integration/type_checking/type_checking_pattern_matrix_size_negative.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (pmatIsCorrectSize pm) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Type checking: Pattern matrix rows have correct size" $ do
               contents <- readFile "test/integration/type_checking/type_checking_pattern_matrix_size_positive.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (pmatIsCorrectSize pm) `shouldSatisfy` isRight
                  _            -> error ""

            it "Type checking: Constructor pattern has wrong arity" $ do
               contents <- readFile "test/integration/type_checking/type_checking_constructor_arity_negative.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pmatConsHaveCorrectArity dts pm) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Type checking: Constructor patterns have correct arity" $ do
               contents <- readFile "test/integration/type_checking/type_checking_constructor_arity_positive.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pmatConsHaveCorrectArity dts pm) `shouldSatisfy` isRight
                  _            -> error ""

            it "Type checking: Pattern has wrong column type" $ do
               contents <- readFile "test/integration/type_checking/type_checking_patterns_right_type_negative.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pmatPatternsAreOfRightType dts pm) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Type checking: Patterns have correct column types" $ do
               contents <- readFile "test/integration/type_checking/type_checking_patterns_right_type_positive.txt"
               case runParserEnd match' contents of

                  ((dts, pm):_) -> (pmatPatternsAreOfRightType dts pm) `shouldSatisfy` isRight
                  _            -> error ""

            it "Type checking: Bound variables multiple declaration in one row" $ do
               contents <- readFile "test/integration/type_checking/type_checking_bound_variables_multiple_declaration_negative.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (pmatVarsUnique pm) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Type checking: Bound variables multiple declaration in separate rows" $ do
               contents <- readFile "test/integration/type_checking/type_checking_bound_variables_multiple_declaration_positive.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (pmatVarsUnique pm) `shouldSatisfy` isRight
                  _            -> error ""

            it "Type checking: Bound variables not defined in all or-pattern branches" $ do
               contents <- readFile "test/integration/type_checking/type_checking_bound_variables_or_branches_different_negative.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (pmatVarsUnique pm) `shouldSatisfy` isLeft
                  _            -> error ""

            it "Type checking: Bound variables defined in all or-pattern branches" $ do
               contents <- readFile "test/integration/type_checking/type_checking_bound_variables_or_branches_identical_positive.txt"
               case runParserEnd match' contents of

                  ((_, pm):_) -> (pmatVarsUnique pm) `shouldSatisfy` isRight
                  _            -> error ""
