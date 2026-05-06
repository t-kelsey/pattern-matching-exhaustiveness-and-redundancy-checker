module Detection where

import UsefulClause
import Parser


-- Check is a pattern matrix is exhaustive under defined data types
-- P is exhaustive if and only if U(P, (_..._)) is false
exhaustive :: DTypes -> PMat -> Bool
exhaustive dts p = not $ (replicate (length $ head p) (PVar "x")) `isUsefulTo` p $ dts

uselessRow :: DTypes -> PMat -> Int -> Bool
uselessRow dts p i = undefined
-- Check if a row in p is useless
