{-# LANGUAGE GADTs #-}
module UsefulClause where

import Parser
import Data.List (sort, nub)


-- Implementation of part of "Warnings for pattern matching" by Luc Maranget


-- The useful clause function, defined by:
-- Let P be a pattern matrix of size m × n and let q be a pattern vector of size n.
-- Vector q is useful with respect to matrix P, if and only if:
--  ∃v, P 'doesn't match' v ∧ q 'matches' v.
useful :: DTypes -> PMat -> PVec -> Bool

-- The base cases of the induction, where n = 0
useful _ [] _ = True -- U({}, q) = True
useful _ _ [] = False -- U((), ()) = False

-- Cases for n > 0:

-- 1. Case constructed pattern q1 = c(p1, p2, ...): U(P, q) <-> U(S(c, P), S(c, q))
useful dts p q@((PCon c rs): qs) = useful dts (specializedP c (length rs) p) specV
    where specV = case specializedV c (length rs) q of
                    (Just sV) -> sV
                    Nothing -> error "This case isn't possible"

-- 2. Case wildcard _:
useful dts p q@(v@(PVar var): qs) = let s = getSigma p 
                              in case isComplete s dts of 

                                -- 2.(a) Case wildcard and sigma is complete: U(P, q) <-> \Or_{k=1}^z useful(S(c_k, P), S(c_k, q))
                                True -> or [useful dts (specializedP c_k (getArity c_k dts) p) (specV c_k) | c_k <- s]
                                    where specV c_k = case specializedV c_k (getArity c_k dts) q of
                                                    (Just sV) -> sV
                                                    Nothing -> error "This case isn't possible"

                                -- 2.(b). Case wildcard and sigma is not complete: U(P, (_ q_2 ... q_n)) = U(D(P), (q_2 ... q_n))
                                False -> useful dts (defaultP p) qs

-- 3. Case or-pattern (r_1 | r_2): U(P, (r_1 | r_2) q_2 ... q_n) = U(P, r_1 q_2 ... q_n) \or U(P, r_2 q_2 ... q_n)
useful dts p q@((POr r_1 r_2): qs) = or [useful dts p (r_1:qs), useful dts p (r_2:qs)]



-- The specialized matrix S(c,P) for constructor patterns. Recursive! The arity of the constructor is needed for the wildcard case.
specializedP :: Constructor -> Int -> PMat -> PMat
specializedP _ _ [] = []
specializedP c a (v:vs) = case v of

    -- 1. Case or-pattern (needs to be handled separately, as it produces two rows)
    ((POr r1 r2):ps) -> (specializedP c a [(r1:ps), (r2:ps)]) ++ (specializedP c a vs)

    -- 2. Every other case gets relegated to the vector version of this function
    _ -> case specializedV c a v of 
        Nothing      -> (specializedP c a vs)
        (Just specV) -> specV : (specializedP c a vs)



-- The specialized vector S(c,v) for constructor patterns. It needs a for the wildcard case.
-- Returns Nothing if No row, returns Just (PVec) else
specializedV :: Constructor -> Int -> PVec -> Maybe PVec 
specializedV c a v = case v of

    -- 1. Case row is constructor pattern 
    ((PCon c' rs):ps) -> if c == c' 
                        then Just (rs ++ ps) -- If the row's con pattern is the same as the 'original' con pattern
                        else Nothing         -- If it isn't
                        -- The maybe is because rs ++ ps can be [] (last column PCon with arity 0)

    -- 2. Case row is wildcard (in our case a var)
    (pv@(PVar v):ps)    -> Just (replicate a pv ++ ps)
    _ -> Nothing


-- 
defaultP :: PMat -> PMat
defaultP [] = []
defaultP (v:vs) = case v of

    -- 1. Case row is constructor pattern
    ((PCon _ _):_) -> defaultP vs -- No row

    -- 2. Case row is wildcard (in our case a var)
    ((PVar _):ps) -> ps : defaultP vs

    -- 3. Case or-pattern
    ((POr r1 r2):ps) -> defaultP [(r1:ps), (r2:ps)] ++ defaultP vs

    _ -> [[]]


-- The set of constructors that appear as root constructors of the patterns of P first column (and or-patterns recursively)
getSigma :: PMat -> [Constructor]
getSigma [] = []
getSigma xs = nub $ getCons xs

        where getCons [] = [] 
              getCons (x:xs) = case x of 
                [] -> []
                (y:ys) -> case y of
                  (PCon c ts) -> c : (getCons xs)
                  (POr r1 r2) -> (getCons [[r1], [r2]]) ++ (getCons xs)
                  _ -> getCons xs


-- Given a constructor, get it's arguments from the data type defs.
getArgsFromCon :: Constructor -> DTypes -> [Type]
getArgsFromCon c dts = 
    
    case [ ts | (_, cds) <- dts, (c', ts) <- cds, c == c' ] of
         (ts':_) -> ts'
         []      -> error $ "\n\n  Type read error: Constructor '" ++ c ++ "' seems to be malformed.\n\n"

-- Get the arity of a constructor based on the data type definitions given
getArity :: Constructor -> DTypes -> Int
getArity = (-1) . length .  getArgsFromCon

-- Get the type a constructor belongs to
getTypeFromCon :: Constructor -> DTypes -> Type
getTypeFromCon c dts = 
    case [ t | (t, cds) <- dts, c `elem` fmap (\(c',_) -> c') cds ] of
        (t:_) -> t
        []    -> error $ "\n\nConstructor " ++ show c ++ " not found in data type definitions given:\n\n" ++ prettyDTypes dts ++ "\n"

-- Get the return type of a constructor
getReturnTypeFromCon :: Constructor -> DTypes -> Type
getReturnTypeFromCon c dts = last $ getArgsFromCon c dts

-- Get the type of a pattern, if possible. Here it suffices to look until we find a constructor, as every pattern in a column
-- has to have the same type. (POr zero nil) is not legal, because zero -> Nat /= nil -> List
getTypeFromPattern :: Pattern -> DTypes -> Maybe Type
getTypeFromPattern (PCon c cs) dts = (Just $ getReturnTypeFromCon c dts)
getTypeFromPattern (PVar _) _ = Nothing
getTypeFromPattern (POr p1 p2) dts = case getTypeFromPattern p1 dts of
                                    (Just t) -> (Just t)
                                    Nothing  -> getTypeFromPattern p2 dts


-- Decides if sigma (set of constructers in first column) is complete within their data type
-- If the first column only has wildcards then obviously sigma is not complete
isComplete :: [Constructor] -> DTypes -> Bool
isComplete [] _ = False
isComplete cs@(c:_) dts = 
    case [ first <$> cds | (t, cds) <- dts, t == getTypeFromCon c dts] of
        (cs': _) -> areSetsEqual cs cs'
        _ -> False
    where first (x, _) = x


areSetsEqual :: (Ord a) => [a] -> [a] -> Bool
areSetsEqual xs ys = sort xs == sort ys


-- More practical infix version
isUsefulTo :: PVec -> PMat -> DTypes -> Bool
(q `isUsefulTo` p) dts = useful dts p q
