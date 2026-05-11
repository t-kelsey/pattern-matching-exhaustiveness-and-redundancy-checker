module Detection where

import UsefulClause
import Parser


-- Check is a pattern matrix is exhaustive under defined data types
-- P is exhaustive if and only if U(P, (_..._)) is false
exhaustive :: DTypes -> PMat -> Bool
exhaustive dts p = not $ (replicate (length $ head p) (PVar "x")) `isUsefulTo` p $ dts

-- Infix version 
isExhaustiveUnder :: PMat -> DTypes -> Bool
p `isExhaustiveUnder` dts = exhaustive dts p

-- Check if a row pi in p is useless by checking if the row is useful to p' where p' only
-- includes rows p0 to p(i-1).
uselessRow :: DTypes -> PMat -> Int -> Bool
uselessRow dts p i = 
    case i >= 0 && i < (length p) of

        True -> not $ p!!i `isUsefulTo` take i p $ dts

        False -> error "Index out of bounds of p"

-- Check if the given pattern matrix contains a useless row
-- P does not have useless rows if UR(P, i) is false for all i
-- Always just gives the last useless row, but all information is there:
-- once the last row is corrected ones before can be found
containsUselessRow :: DTypes -> PMat -> Maybe PVec
containsUselessRow dts p = checkRows ((length p) - 1)

    where checkRows :: Int -> Maybe PVec
          checkRows (-1) = Nothing
          checkRows i =

            case uselessRow dts p i of
                True -> Just (p!!i)
                False -> checkRows (i-1)

-- This is the main function of this project. The output is the combination of warnings 
-- generated, and of course the exhaustiveness check.
warnings :: DTypes -> PMat -> String
warnings dts p = case semanticTestsTextGen of
        "" -> do
            let header = "\n\n--- EXHAUSTIVENESS AND REDUNDANCY CHECKER OUTPUT ---"
            let footer = "\n\n----------------------------------------------------"
            -- here extensions such as overcomplicated cases
            -- ocCasesText = ocCasesTextGen

            header ++ isExTextGen ++ urTextGen ++ footer
        errorText -> errorText

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

          semanticTestsTextGen :: String
          semanticTestsTextGen = ""

          
