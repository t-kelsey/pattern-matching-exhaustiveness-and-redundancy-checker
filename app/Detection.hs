module Detection where

import UsefulClause
import Parser
import Data.List (sort, transpose, nub, (\\), intersperse)
-- import Control.Applicative (liftA2)


-- Check is a pattern matrix is exhaustive under defined data types
-- P is exhaustive if and only if U(P, (_..._)) is false
exhaustive :: DTypes -> PMat -> Bool
exhaustive dts p = not $ (replicate (length $ head p) (PVar "x")) `isUsefulTo` p $ dts

-- The witness of the exhaustive function. Returns an example row that is missing in the matrix.
-- There is an example row e iff P is not exhaustive. e is defined U(P, e) = True. 
-- Most importantly, Exhaustive(P@e) is NOT defined as true, as that is not the aim of the function I
-- defined in the paper.
witness :: DTypes -> PMat -> Either False PVec
witness = undefined

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

    dtypeConReturnsType dts             -- Each constructor should returns the type it is supposed to construct
    dtypeTypesExist dts                 -- Each data type used in a definition actually exists
    dtypeNamesUnique dts                -- Each defined type and constructor name is unique
    pmatConsExist dts pmat              -- Each type used in the pattern matrix is defined
    pmatIsCorrectSize pmat              -- Ensure matrix if of width n, no row is longer or shorter
    pmatConsHaveCorrectArity dts pmat   -- Ensure that each constructor used in the pattern matrix has the correct number of arguments applied
    pmatPatternsAreOfRightType dts pmat -- Ensure each pattern in a column is of the same type as the column
    pmatVarsUnique pmat                 -- Ensure each variable used is either unique or is in the same level of an or-pattern


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
            
            if getArity dts c == length cs

                then rowArity xs

                else 
                    case getArity dts c > length cs of

                        True -> (Left $ "\n\n  Type read error: Constructor '" ++ show c ++ "' in \n\n  '" 
                                ++ prettyPVec (pat:xs) ++ "'\n\n  has to few arguments.\n\nExpected:  " 
                                ++ prettyConstrDec (c, getArgsFromCon dts c) ++ "\nActual:    " 
                                ++ prettyConstrDec (c, ((getTypeFromPattern' dts) <$> cs) ++ [getReturnTypeFromCon dts c]) ++ "\n\n")

                        False -> (Left $ "\n\n  Type read error: Constructor '" ++ show c ++ "' in \n\n  '" 
                                ++ prettyPVec (pat:xs) ++ "'\n\n  has to many arguments.\n\nExpected:  " 
                                ++ prettyConstrDec (c, getArgsFromCon dts c) ++ "\nActual:    " 
                                ++ prettyConstrDec (c, ((getTypeFromPattern' dts) <$> cs) ++ [getReturnTypeFromCon dts c]) ++ "\n\n")

          -- Case 2: pattern is a variable
          rowArity ((PVar _):xs) = rowArity xs

          -- Case 3: pattern is an or-pattern
          rowArity ((POr p1 p2):xs) = case rowArity (p1:p1:[]) of

                                        (Right ()) -> rowArity xs
                                        (Left s)   -> (Left s)


-- Ensure each pattern in a column is of the same type as the column
pmatPatternsAreOfRightType :: DTypes -> PMat -> Either String ()
pmatPatternsAreOfRightType dts pmat@(r1:pm) = foldr propagate (Right ()) pm

    where propagate ri (Right ()) = compareRow r1 ri (getColumnBindings dts pmat) ri
          propagate _   (Left s)   = (Left s)

          -- We check the binding of the first row, then compare that binding to each individual row
          compareRow []       _        _        _       = (Right ())
          -- r1: first row, ri: current compare row, cbs: column bindings. Need r1 for error message only
          compareRow (p1:r1') (pi:ri') (cb1:cbs) fullri = 
            
            if case (getTypeFromPattern dts pi) of
                
                -- If the current pattern can be bound, compare it to the column type
                (Just t) -> (Just t) == cb1

                -- If it can't be bound, it's a variable (or Or-Pattern with only vars) and such is of the right type
                Nothing  -> True
                                           
            then compareRow r1' ri' cbs fullri
                                           
            else (Left $ "\n\n  Type read error: Pattern '" ++ prettyP pi ++ "', in \n  '" 
                                ++ prettyPVec (fullri) ++ "', is of wrong type.\n\n" 
                                ++ "  Actual:    '" ++ prettyPT pi ++ prettyIndirectBind p1 cb1 cbs ++ "\n\n" )

        
          -- Better error messages
          prettyIndirectBind p1' cb1' cbs' = 
            case (getTypeFromPattern dts p1') == cb1' of
                True -> "'\n  Expected:    '" 
                                ++ prettyTypeMaybe cb1' ++ "',  bound at:  '"
                                ++ prettyP p1' ++ " :: " ++ prettyTypeMaybe cb1' ++ "'\n  In the first row of the pattern matrix:\n  '" 
                                ++ prettyPVec (r1) ++ "'\n"

                False -> "'\n Expected:    '"
                                ++ prettyTypeMaybe cb1' ++ "', bound at:  '"
                                ++ prettyP (head (dropWhile (\x -> getTypeFromPattern' dts x == "?") ((transpose pmat) !! ((length (head pmat) - length cbs') - 1))))
                                ++ "'\n"

          prettyPT p = prettyType (getTypeFromPattern' dts p)

          prettyTypeMaybe (Just t) = t
          prettyTypeMaybe (Nothing) = "?"



-- Get the binding of each column
getColumnBindings :: DTypes -> PMat -> [Maybe Type]
getColumnBindings dts pm = getColumnBinding <$> transpose pm -- Transposed so it goes column by column, not row by row
    

    where getColumnBinding [] = Nothing
          getColumnBinding (p1:ci) =

            case getTypeFromPattern dts p1 of

                (Just t) -> (Just t) -- If the first pattern is bindable, return it
                Nothing  -> getColumnBinding ci -- If not, move a row down


-- Turn the 'maybe' into a type, which has an implicit show instance
getTypeFromPattern' :: DTypes -> Pattern -> Type
getTypeFromPattern' dts p = case getTypeFromPattern dts p of
                                            (Just t) -> t
                                            Nothing  -> "?"


-- Ensure each variable used is either unique or is in the same level of an or-pattern
-- Here we have to define our method ourself, as I could not find literature that does

-- First, we define that variables in the patterns of a pattern matrix P must fulfill a uniqueness property.

pmatVarsUnique :: PMat -> Either String ()
pmatVarsUnique pm = foldr propagate (Right ()) pm

    where propagate ri (Right ()) = do

                let vars = getVars ri
                checkUnique vars ri       -- Check if the final possibilities all have unique vars
                checkBranches vars ri     -- Check if the final possibilities all have the same vars

          propagate _  (Left s)   = (Left s)

          getVars :: [Pattern] -> [[Pattern]]

          -- Let's define variables as v_i, and patterns as p_i 

          -- The most important step is the recursive reduction function R, 
          -- which holds for unique(p_1) == unique(p_2) iff unique(R(p_1)) == unique(R(p_2))

          -- R is defined as:   R(c(r_1,...,r_a)) ~= R(r_1) ... R(r_a)
          --                    R(v)              ~= v
          --                    R( r_1 | r_2 )    ~= ( R(r_1) | R(r_2) )   R does not have the power to solve the or itself 

          -- Now, getVars is an extension of R used to solve the or-pattern problem. We increase the solvability by
          -- reducing with R whenever we reach an or-pattern, that way we can use boolean logic to carve out the specific 
          -- cases correctly. getVars returns a matrix where the rows are 'possibilities', each representing a branch of an or. 
          --                  
          -- getVars also does the induction over the columns for each row.
          
          -- Base case: no column (n=0) -> getVars(()) = ()
          getVars [] = [[]]
           
          -- Case 1: If p_1 is a constructor pattern, we can just reduce it. If we have multiple possiblities after reducing, we need the
          -- cartesian product so each possibility gets correctly updated. 
          getVars ((PCon _ cs):ri) = (liftA2 (++)) (getVars cs) (getVars ri)

          -- Case 2: If the pattern is a var, just add it to the list. Here again we map over possibilities
          getVars (pv@(PVar _):ri) = ((:) pv) <$> (getVars ri)

          -- If the pattern is an or-pattern, it's more complicated. The same variables have to be defined on both sides.
          -- So whenever there is an or-pattern, we split the branches to compare individually, and the make sure each branch
          -- has the same vars. The logic is as follows:

          -- unique( (p_1 | p_2) ) = unique( R(p_1) ) && unique( R(p_2) ) && R(p_1) `setEqual` R(p_2) 

          -- Case 3: If the pattern is an or-pattern, then create two new possibilities in the matrix.
          getVars ((POr p1 p2):ri) = (liftA2 (++)) (getVars [p1] ++ getVars [p2]) (getVars ri)



          -- Here we finally define unique. r_i is row i, the induction is over the rows

          -- Base cases, if no row: unique(()) = True

          -- Inductive case: unique((r_i)) = True iff for all v_i, v_j in R(r_i), v_i /= v_j && unique((r_i-1))
          checkUnique :: [[Pattern]] -> [Pattern] -> Either String ()
          checkUnique [] _ = (Right ())
          checkUnique (r:rs) stacktrace = case r == (nub r) of

            True  -> checkUnique rs stacktrace
            False -> (Left $ "\n\n  Multiple declaration of variable '" ++ prettyP (head (r \\ (nub r))) ++ "'\n\n  in: '" ++ prettyPVec stacktrace ++ "' of the pattern matrix.\n\n")

          -- This checks the possibilities, to make sure that all variables are defined in all possibilities.
          -- That is equivalent to all or-patterns having the same variables bound on both sides.
          checkBranches :: [[Pattern]] -> [Pattern] -> Either String ()
          checkBranches [] _ = (Right ())
          checkBranches (_:[]) _ = (Right ())
          checkBranches (r1:r2:rs) stacktrace = case r1 == r2 of

            True  -> checkBranches (r2:rs) stacktrace
            False -> (Left $ "\n\n Variables '" ++ (intersperse ',' $ prettyP (head (r1 \\ r2)) ++ prettyP (head (r2 \\ r1))) ++ "' are not defined in all branches of or-pattern"
                      ++ "\n\n  in: '" ++ prettyPVec stacktrace ++ "' of the pattern matrix.\n\n")