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

          
