module Main where

import Parser
import Detection

main :: IO ()
main = do
    contents <- readFile "resources/input.txt"
    case runParserEnd match' contents of

        []              -> putStrLn $ findParseError contents -- serror "\n\nParser Error: runParserEnd could not match test.txt. Check your syntax.\n"
        -- If the datatypes and pattern matrix  parse, run the warnings function on them
        ((dts, p, _):_) -> putStrLn (warnings dts p)



-- This is the main function of this project. The output is the combination of warnings 
-- generated, semantic input checking, and of course the exhaustiveness check.
warnings :: DTypes -> PMat -> String
warnings dts p = case typeCheck dts p of

        (Right ()) -> do
            let header = "\n\n--- EXHAUSTIVENESS AND REDUNDANCY CHECKER OUTPUT ---"
            let footer = "\n\n----------------------------------------------------"
            -- here extensions such as overcomplicated cases
            -- ocCasesText = ocCasesTextGen

            header ++ isExTextGen ++ urTextGen ++ footer

        (Left errorText) -> errorText

    where 
          isExTextGen :: String
          isExTextGen =
            case p `isExhaustiveUnder` dts of

                True -> "\n\n    Success: Cases are exhaustive"
                False -> "\n\n    Failure: Cases are not exhaustive" ++ casesForExTextGen

          urTextGen :: String
          urTextGen =
            case containsUselessRow dts p of

                (Just pv) -> "\n\n    '" ++ prettyPVec pv ++ "'\n    ^ This case in the pattern matrix is redundant"
                Nothing -> ""

          casesForExTextGen :: String
          casesForExTextGen = "\n\n    Maybe you forgot this case: Not implemented yet"


findParseError :: String -> String 
findParseError contents = undefined


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
