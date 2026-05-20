module Detection where

import UsefulClause
import Parser
import Data.List (sort)


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


-- Type Checking functions (Semantic tests functions for the parser)
-- This makes sure the input is viable and already takes some work from the compiler
-- by preprocessing things like variable handling so the compiler doesn't have to worry about it
typeCheck :: DTypes -> PMat -> Either String ()
typeCheck dts pmat = do

    dtypeConReturnsType dts     -- Each constructor should returns the type it is supposed to construct
    dtypeTypesExist dts         -- Each data type used in a definition actually exists
    dtypeNamesUnique dts        -- Each defined type and constructor name is unique
    pmatConsExist dts pmat      -- Each type used in the pattern matrix is defined
    pmatIsCorrectSize pmat      -- Ensure matrix if of width n, no row is longer or shorter



-- Make sure that each constructor returns the type it is supposed to construct
dtypeConReturnsType :: DTypes -> Either String ()
dtypeConReturnsType dts = foldr propagate (Right ()) [ (t, c, ts) | (t, cds) <- dts, (c, ts) <- cds]

    -- Propagate the intermediary result along the fold - The first error always gets passed to the end
    where propagate (t', c', ts') (Right ()) = if t' == last ts'
                                               then (Right ())
                                               else (Left ("\n\n  Type read error: In type '" ++ t' ++ "', constructor '" ++ 
                                                    prettyConstrDec (c', ts') ++ "' doesn't return type '" ++ t' ++ "'!\n"))

          propagate _ (Left s) = (Left s)


-- Make sure that each data type used in a definition actually exists
dtypeTypesExist :: DTypes -> Either String ()
dtypeTypesExist dts = foldr propagate (Right ()) [ (t, et) | (et, cds) <- dts, (_, ts) <- cds, t <- ts]

    where propagate (t', et') (Right ()) = if t' `elem` ts'
                                    then (Right ())
                                    else (Left ("\n\n  Type read error: In type '" ++ et' ++ "', type '" ++ t' ++ "' is not defined.\n"))
          propagate _ (Left s) = (Left s)

          ts' = [ t'' | (t'', _) <- dts]


-- Make sure no constructors or types have the same names
dtypeNamesUnique :: DTypes -> Either String ()
dtypeNamesUnique dts = 
    case foldr propagate (Right "") conAndTypeNames of

        (Right _) -> (Right ())
        (Left s)  -> (Left s)
          
    where propagate :: String -> Either String String -> Either String String
          propagate c' (Right s) = if c' == s
                                   then (Left ("\n\n  Type read error: Multiple declaration of '" ++ c' ++ "'!\n"))
                                   else (Right c')
          propagate _ (Left s) = (Left s)

          -- A list containing the name of every con and type
          conAndTypeNames = sort ([ c | (_, cds) <- dts, (c, _) <- cds] ++ [ t | (t, _) <- dts])


-- Make sure each constructor used in the pattern matrix actually exists
pmatConsExist :: DTypes -> PMat -> Either String ()
pmatConsExist dts pm = foldr propagate (Right ()) pm
 
    where propagate row (Right ()) = rowElemDTypes row
          propagate _ (Left s) = (Left s)

          -- Here we pull apart each row into patterns, check each pattern recursively, and pass
          -- the intermediary results of each pattern into the next pattern recursively. The individual
          -- row results gets propagated then finally in the same way for a 3-layer-deep-fold

          -- Base case
          rowElemDTypes [] = (Right ())

          -- Case 1: pattern is a constructor pattern
          rowElemDTypes (pat@(PCon c cs):xs) = if c `elem` cons
                                               then case rowElemDTypes cs of

                                                      (Right ()) -> rowElemDTypes xs
                                                      (Left s)   -> (Left s)

                                               else (Left $ "\n\n Type read error: Constructor '" ++ c ++ "', in \n\n'" 
                                                    ++ prettyPVec (pat:xs) ++ "'\n\ndoes not exist in the given data types.\n\n")

          -- Case 2: pattern is a variable
          rowElemDTypes ((PVar _:xs)) = rowElemDTypes xs

          -- Case 3: pattern is an or-pattern
          rowElemDTypes ((POr x1 x2):xs) = case rowElemDTypes (x1:x2:[]) of
                                             
                                             (Right ()) -> rowElemDTypes xs
                                             (Left s)   -> (Left s)


          -- Each constructor defined in DTypes
          cons = [ con | (et, cds) <- dts, (con, ts) <- cds]

-- Make sure that the pattern matrix is of shape n * m
pmatIsCorrectSize :: PMat -> Either String ()
pmatIsCorrectSize (r1:pm) = foldr propagate (Right ()) pm 

    where propagate row' (Right ()) = if length row' == n
                                      then (Right ())
                                      else (Left $ "\n\n  Type read error:\n\n  Row '" ++ prettyPVec row' 
                                           ++ "'\n\n  Has length " ++ show (length row') ++ ", yet pattern matrix is of width " ++ show n ++ ".\n\n")

          propagate _ (Left s) = (Left s)

          n = length r1 

-- Make sure that each constructor used in the pattern matrix has the correct number of arguments applied
pmatConsHaveCorrectArity :: DTypes -> PMat -> Either String ()
pmatConsHaveCorrectArity dts pm = foldr propagate (Right ()) pm

    where propagate row (Right ()) = rowArity row
          propagate _ (Left s) = (Left s)

          -- Pull apart each row into it's patterns, then each pattern into it's non-recursive parts
          -- Check for each pattern then the arity if it's a constructor

          -- Base case
          rowArity [] = (Right ())
          
          -- Case 1: pattern is a constructor pattern
          rowArity (pat@(PCon c cs):xs) = 
            
            if getArity c + 1 == length cs

                then rowArity xs

                else 
                    case getArity c + 1 > length cs of

                        True -> (Left $ "\n\n  Type read error: Constructor '" ++ show c ++ "' in \n\n  '" 
                                ++ prettyPVec (pat:xs) ++ "'\n\nhas to few arguments.\n\nExpected:  " 
                                ++ prettyConstrDec (c, getArgsFromCon dts c) ++ "\nActual:  " ++ prettyConstrDec (c, getTypeFromPattern <$> cs) ++ "\n\n")

                        False -> (Left $ "")

          -- Case 2: pattern is a variable
          rowArity ((PVar _):xs) = rowArity xs

          -- Case 3: pattern is an or-pattern
          rowArity ((POr p1 p2):xs) = case rowArity (p1:p1:[]) of

                                        (Right ()) -> rowArity xs
                                        (Left s)   -> (Left s)
