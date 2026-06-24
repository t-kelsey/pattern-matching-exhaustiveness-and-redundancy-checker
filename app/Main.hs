module Main where

import Parser
import Detection
import Data.List (intercalate, transpose)

main :: IO ()
main = do
    contents <- readFile "resources/input.txt"
    case findParseError contents of

        (Right ())  -> -- No parse errors detected
            case runParserEnd match' contents of
            
            []      -> putStrLn "\n\n  Parse failure: Could not match input.txt. Check your syntax.\n"

            -- If the datatypes and pattern matrix parse, run the warnings function on them
            ((dts, pm, _):_) -> putStrLn (warnings dts pm)
            

        (Left errorText) -> putStrLn errorText



-- This is the main function of this project. The output is the combination of warnings 
-- generated, semantic input checking, and of course the exhaustiveness check.
warnings :: DTypes -> PMat -> String
warnings dts pm = case typeCheck dts pm of

        (Right ()) -> do
            let header = "\n\n--- EXHAUSTIVENESS AND REDUNDANCY CHECKER OUTPUT ---"
            let footer = "\n\n----------------------------------------------------"
            -- here extensions such as overcomplicated cases
            -- ocCasesText = ocCasesTextGen

            header ++ isExTextGen ++ vbTextGen ++ urTextGen ++ footer

        (Left errorText) -> errorText

    where 
          -- Returns the text for checking exhaustiveness
          isExTextGen :: String
          isExTextGen =
            case pm `isExhaustiveUnder` dts of

                True -> "\n\n    Success: Cases are exhaustive"
                False -> "\n\n    Failure: Cases are not exhaustive" ++ casesForExTextGen

          -- Returns the text for how each variable was bound
          vbTextGen :: String
          vbTextGen = "\n\n    with bindings  \n      column 1     column 2  ... \n      " ++ intercalate "\n      " (convertVarBindsToString <$> (transpose $ getVarBindings dts pm))

          convertVarBindsToString [] = []
          convertVarBindsToString ((v, mt):xs) = 
            case mt of

                (Just t) -> v ++ ": " ++ t ++ ",     " ++ convertVarBindsToString xs
                Nothing   -> v ++ ": ?" ++ ",       " ++ convertVarBindsToString xs

          -- Returns the text for redundant cases, also called useless rows
          urTextGen :: String
          urTextGen =
            case containsUselessRow dts pm of

                (Just pv) -> "\n\n    '" ++ prettyPVec pv ++ "'\n    ^ This case in the pattern matrix is redundant"
                Nothing -> "\n\n    No cases are redundant."

          casesForExTextGen :: String
          casesForExTextGen = "\n\n    Maybe you forgot this case: " ++ case witness dts pm (length $ head pm) of
                                                                            (Left ()) -> "Could not find missing case."
                                                                            (Right pv) -> prettyPVec pv


findParseError :: String -> Either String ()
findParseError s = do

    checkSectionHeaders s -- Ensure the dividers of the sections are correctly in place
    checkEmptySections s  -- Ensure each section isn't empty
    checkDTypes s         -- Ensure no data types are malfomed 
    checkPMat s           -- Ensure the pattern matrix is not malformed
    checkVVec s           -- Ensure the types (values) given aren't malformed


-- data Nat where
--     Zero :: Nat
--     Succ :: Nat -> Nat

-- ex :: Nat -> Nat -> Nat
-- ex (Succ x) x = Succ x
-- ex _ x = x

-- Extension idea 1: Variables instead of Wildcards. Requires Free & Bound defs. Not sure how hard it is.
-- This goes farther than haskell does, I think? Haskell doesn't allow same vars in defs, example:
-- ex :: Nat -> Nat -> Nat
-- ex (succ x) x = ...   haskell "Conflicting definition"
-- ex (succ x) y = ...   fine
-- Added some tests, we'll see

-- Extension idea 2: Desugaring partial application: Allow "((succ succ) zero)" instead of just "(succ (succ zero))" 
--     -> maybe not a good idea. Even haskell says "nah". But in lambda calc it's ok.

-- Extension idea 3: "Maybe you forgot this case: [...]?" needs I as defined in the paper, should be easy

-- Extension idea 4: "This is a useless clause: [...]" already implemented, but can be better with specialization of U as in paper

-- Extension idea 5: Desugaring wildcard constructors: 
--          Allow "(_ zero)" which desugars to "zero | (suc zero)",
--          Allow "(succ | pred zero)" -> may require explicit parenthesization in or-patterns. Should be easy.

-- Extension idea 6: Semantic checking after parse, "This data type is malformed: xxx"

-- Extension idea 7: "Safeguards" as defined by the paper against exponential runtime in real applications

-- TO-DO: 
-- tests
-- in witness: unique variable declaration
-- check bound variables again
-- display how each var is bound
-- redundancy
