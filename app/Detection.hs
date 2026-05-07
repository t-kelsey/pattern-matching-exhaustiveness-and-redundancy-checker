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
    case i >= 0 && i < (length dts) of

        True -> p!!i `isUsefulTo` p $ dts 

        False -> error "Index out of bounds of p"

