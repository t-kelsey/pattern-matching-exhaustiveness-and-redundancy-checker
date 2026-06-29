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
               contents <- readFile "test/integration/parsing/parser_data_types.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (prettyDType $ head dts) `shouldBe` "Unit\n  tt : Unit"

            it "Parser: pattern matrix is correctly parsed" $ do
               contents <- readFile "test/integration/parsing/parser_pattern_matrix_or_constructor_rows.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (prettyPMat pm) `shouldBe` "((nat x zero y) | (nat zero zero nil)) x\n(list (cons zero nil)) x\n(unit x) x"

            it "Parser: upper-case constructor pattern row is correctly parsed" $ do
               contents <- readFile "test/integration/parsing/parser_pattern_row_uppercase_constructors.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugPVec $ head pm) `shouldBe` "[(PCon OneOfThose []), (PCon Nat [])]"

            it "Parser: test file is correctly loaded and parsed" $ do
               contents <- readFile "test/integration/parsing/parser_full_match_unit_nat_list_one_of_those.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (length dts, length pm) `shouldBe` (4, 3)

            -- Parser tests: Implicit and explicit data structures should be parsed correctly

            -- Con tests
            it "Parser: Constructor pattern with > 0 arity is handled correctly" $ do
               contents <- readFile "test/integration/parsing/parser_constructor_pattern_nat_with_three_arguments.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugP $ head $ head pm) `shouldBe` "(PCon nat [(PCon zero []), (PCon succ [(PCon zero [])]), (PCon nil [])])"

            it "Parser: Constructor pattern with = 0 arity is handled correctly" $ do
               contents <- readFile "test/integration/parsing/parser_constructor_pattern_nested_successor.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugP $ head $ head pm) `shouldBe` "(PCon succ [(PCon succ [(PCon zero [])])])"

            -- Var tests
            it "Parser: Variable pattern is correctly positively identified" $ do
               contents <- readFile "test/integration/parsing/parser_variable_pattern_lowercase_x.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugP $ head $ head pm) `shouldBe` "(PCon succ [(PCon succ [(PVar x)])])"

            it "Parser: Variable pattern is correctly negatively identified" $ do
               contents <- readFile "test/integration/parsing/parser_constructor_pattern_uppercase_x.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugP $ head $ head pm) `shouldBe` "(PCon succ [(PCon succ [(PCon X [])])])"

            -- Or tests
            it "Parser: Or pattern is correctly identified" $ do
               contents <- readFile "test/integration/parsing/parser_or_pattern_zero_or_one.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugP $ head $ head pm) `shouldBe` "(POr (PCon zero []) | (PCon one []))"

            it "Parser: Or pattern implicit parenthesization is correct on rows" $ do
               contents <- readFile "test/integration/parsing/parser_or_pattern_implicit_row_parentheses.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugPVec $ head pm) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"

            it "Parser: Or pattern explicit parenthesization is correct on rows" $ do
               contents <- readFile "test/integration/parsing/parser_or_pattern_explicit_row_parentheses.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugPVec $ head pm) `shouldBe` "[(POr (PCon zero []) | (PCon one [])), (PCon zero [])]"

            it "Parser: Or pattern is correctly identified in constructor pattern" $ do
               contents <- readFile "test/integration/parsing/parser_or_pattern_inside_successor_constructor.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugP $ head $ head pm) `shouldBe` "(PCon succ [(POr (PCon zero []) | (PCon one []))])"

            it "Parser: Or pattern is correct in nested or-patterns with mixed implicit and explicit parenthesization" $ do
               contents <- readFile "test/integration/parsing/parser_nested_or_pattern_zero_one_two.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (debugP $ head $ head pm) `shouldBe` "(POr (PCon zero []) | (POr (PCon one []) | (PCon two [])))"

            -- Parser semantic tests
            it "Parser semantic: each constructor returns the type it is supposed to positive" $ do
               contents <- readFile "test/integration/parsing/parser_semantic_constructor_returns_type_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (dtypeConReturnsType dts) `shouldSatisfy` isRight

            it "Parser semantic: each constructor returns the type it is supposed to negative" $ do
               contents <- readFile "test/integration/parsing/parser_semantic_constructor_returns_type_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (dtypeConReturnsType dts) `shouldSatisfy` isLeft

            -- Useful clause tests: Individual function tests
            it "Useful clause: getSigma works correctly on edge cases" $ do
               contents <- readFile "test/integration/algorithm/useful_get_sigma_collects_constructor_names_from_or_patterns.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (intercalate ", " (getSigma pm)) `shouldBe` "succ, zero, two, three, four"

            it "Useful clause: specializedP (V) works correctly on edge cases" $ do
               contents <- readFile "test/integration/algorithm/useful_specialized_p_expands_cons_constructor_rows.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (prettyPMat (specializedP "cons" 2 pm)) `shouldBe` "one nil nil\nzero nil nil\ny y nil\nx x nil"

            it "Useful clause: defaultP works correctly on edge cases" $ do
               contents <- readFile "test/integration/algorithm/useful_default_p_keeps_variable_and_nonmatching_rows.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (prettyPMat (defaultP pm)) `shouldBe` "nil\nnil"

            it "Useful clause: Main function test negative example" $ do
               contents <- readFile "test/integration/algorithm/useful_clause_nat_zero_zero_is_not_useful.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (head pm `isUsefulTo` tail pm $ dts) `shouldBe` False

            it "Useful clause: Main function test positive example" $ do
               contents <- readFile "test/integration/algorithm/useful_clause_nat_zero_x_nil_is_useful.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (head pm `isUsefulTo` tail pm $ dts) `shouldBe` True

            it "Detection: exhaustive function general test negative example" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_one_of_those_matrix_is_not_exhaustive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pm `isExhaustiveUnder` dts) `shouldBe` False

            it "Detection: exhaustive function general test positive example" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_one_of_those_matrix_with_wildcard_is_exhaustive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pm `isExhaustiveUnder` dts) `shouldBe` True

            it "Detection: exhaustive function test edge case not all constructors used of data type" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_fruit_missing_constructor_is_useful.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (head pm `isUsefulTo` tail pm $ dts) `shouldBe` True

            it "Detection: exhaustive function test edge case all constructors used of data type" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_fruit_all_constructors_used_is_not_useful.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (head pm `isUsefulTo` tail pm $ dts) `shouldBe` False

            it "Detection: exhaustive function test edge case constructor exhaustion" $ do
               contents <- readFile "test/integration/algorithm/detection_exhaustiveness_fruit_constructor_exhaustion.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (head pm `isUsefulTo` tail pm $ dts) `shouldBe` False

            it "Detection: contains useless row function positive example" $ do
               contents <- readFile "test/integration/algorithm/detection_contains_useless_row_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (containsUselessRow dts pm) `shouldBe` (Just $ last pm)

            it "Detection: contains useless row function negative example" $ do
               contents <- readFile "test/integration/algorithm/detection_contains_useless_row_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (containsUselessRow dts pm) `shouldBe` Nothing

            it "Type checking: Constructor multiple declaration" $ do
               contents <- readFile "test/integration/type_checking/type_checking_constructor_multiple_declaration_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (dtypeNamesUnique dts) `shouldSatisfy` isLeft

            it "Type checking: Constructor multiple declaration" $ do
               contents <- readFile "test/integration/type_checking/type_checking_constructor_multiple_declaration_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (dtypeNamesUnique dts) `shouldSatisfy` isRight

            it "Type checking: Referenced type is not defined" $ do
               contents <- readFile "test/integration/type_checking/type_checking_dtype_types_exist_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (dtypeTypesExist dts) `shouldSatisfy` isLeft

            it "Type checking: Referenced types are defined" $ do
               contents <- readFile "test/integration/type_checking/type_checking_dtype_types_exist_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (dtypeTypesExist dts) `shouldSatisfy` isRight

            it "Type checking: Pattern constructor is not defined" $ do
               contents <- readFile "test/integration/type_checking/type_checking_pattern_constructor_exists_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatConsExist dts pm) `shouldSatisfy` isLeft

            it "Type checking: Pattern constructors are defined" $ do
               contents <- readFile "test/integration/type_checking/type_checking_pattern_constructor_exists_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatConsExist dts pm) `shouldSatisfy` isRight

            it "Type checking: Pattern matrix row has wrong size" $ do
               contents <- readFile "test/integration/type_checking/type_checking_pattern_matrix_size_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatIsCorrectSize pm) `shouldSatisfy` isLeft

            it "Type checking: Pattern matrix rows have correct size" $ do
               contents <- readFile "test/integration/type_checking/type_checking_pattern_matrix_size_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatIsCorrectSize pm) `shouldSatisfy` isRight

            it "Type checking: Constructor pattern has wrong arity" $ do
               contents <- readFile "test/integration/type_checking/type_checking_constructor_arity_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatConsHaveCorrectArity dts pm) `shouldSatisfy` isLeft

            it "Type checking: Constructor patterns have correct arity" $ do
               contents <- readFile "test/integration/type_checking/type_checking_constructor_arity_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatConsHaveCorrectArity dts pm) `shouldSatisfy` isRight

            it "Type checking: Pattern has wrong column type" $ do
               contents <- readFile "test/integration/type_checking/type_checking_patterns_right_type_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatPatternsAreOfRightType dts pm) `shouldSatisfy` isLeft

            it "Type checking: Patterns have correct column types" $ do
               contents <- readFile "test/integration/type_checking/type_checking_patterns_right_type_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatPatternsAreOfRightType dts pm) `shouldSatisfy` isRight

            it "Type checking: Bound variables multiple declaration in one row" $ do
               contents <- readFile "test/integration/type_checking/type_checking_bound_variables_multiple_declaration_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatVarsUnique pm) `shouldSatisfy` isLeft

            it "Type checking: Bound variables multiple declaration in separate rows" $ do
               contents <- readFile "test/integration/type_checking/type_checking_bound_variables_multiple_declaration_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatVarsUnique pm) `shouldSatisfy` isRight

            it "Type checking: Bound variables not defined in all or-pattern branches" $ do
               contents <- readFile "test/integration/type_checking/type_checking_bound_variables_or_branches_different_negative.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatVarsUnique pm) `shouldSatisfy` isLeft

            it "Type checking: Bound variables defined in all or-pattern branches" $ do
               contents <- readFile "test/integration/type_checking/type_checking_bound_variables_or_branches_identical_positive.txt"
               let ((dts, pm):_) = runParserEnd match' contents

               (pmatVarsUnique pm) `shouldSatisfy` isRight
