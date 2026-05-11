module Main where

import Parser
import UsefulClause
import Detection

main :: IO ()
main = do
    contents <- readFile "resources/input.txt"
    case runParserEnd match' contents of

        []              -> error "\n\nParser Error: runParserEnd could not match test.txt. Check your syntax.\n"

        ((dts, p, _):_) -> putStrLn (warnings dts p)


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
