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
useful _ _ [] = False -- U((), ()) = False
useful _ [] _ = True -- U({}, q) = True

-- Cases for n > 0:

-- 1. Case constructed pattern q1 = c(p1, p2, ...): U(P, q) <-> U(S(c, P), S(c, q))
useful dts p q@((PCon c rs): qs) = useful dts (specializedP c (length rs) p) (specializedV c (length rs) q)

-- 2. Case wildcard _:
useful dts p q@(v@(PVar var): qs) = let s = getSigma p 
                              in case isComplete s dts of 

                                -- 2.(a) Case wildcard and sigma is complete: U(P, q) <-> \Or_{k=1}^z useful(S(c_k, P), S(c_k, q))
                                True -> or [useful dts (specializedP c_k (getArity c_k dts) p) (specializedV c_k (getArity c_k dts) q) | c_k <- s]

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
    _ -> if specV == [] then (specializedP c a vs) else specV : (specializedP c a vs)
            where specV = specializedV c a v


-- The specialized vector S(c,v) for constructor patterns. It needs a for the wildcard case.
specializedV :: Constructor -> Int -> PVec -> PVec 
specializedV c a v = case v of

    -- 1. Case row is constructor pattern 
    ((PCon c' rs):ps) -> if c == c' 
                        then (rs ++ ps) -- If the row's con pattern is the same as the 'original' con pattern
                        else []         -- If it isn't

    -- 2. Case row is wildcard (in our case a var)
    (pv@(PVar v):ps)    -> (replicate a pv ++ ps)
    _ -> []


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


-- The set of constructors that appear as root constructors of the patterns of P first column (and or-patterns recursively)
getSigma :: PMat -> [Constructor]
getSigma xs = nub $ getCons xs
    where getCons (x:xs) = case head x of 
             (PCon c ts) -> c : (getCons xs)
             (POr r1 r2) -> (getCons [[r1], [r2]]) ++ (getCons xs)
             _ -> []
          getCons [] = []

-- Get the arity of a constructor based on the data type definitions given
getArity :: Constructor -> DTypes -> Int
getArity c dts = 
    case [ ts | (_, cds) <- dts, (c', ts) <- cds, c == c' ] of
        (ts: _)-> length ts
        [] -> error "\n\nNo match Found\n\n"

-- Get the type a constructor belongs to
getTypeFromCon :: Constructor -> DTypes -> Type
getTypeFromCon c dts = 
    case [ t | (t, cds) <- dts, c `elem` fmap (\(c',_) -> c') cds ] of
        (t:_) -> t
        []    -> error "\n\nNo match Found\n\n"


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



-- Extension idea 1: Variables instead of Wildcards. Requires Free & Bound defs. Not sure how hard it is.

-- Extension idea 2: Desugaring partial application: Allow "((succ succ) zero)" instead of just "(succ (succ zero))" 
--     -> maybe not a good idea. Even haskell says "nah". But in lambda calc it's ok.

-- Extension idea 3: "Maybe you forgot this case: [...]?"

-- Extension idea 4: Desugaring wildcard constructors: 
--          Allow "(_ zero)" which desugars to "zero | (suc zero)",
--          Allow "(succ | pred zero)" -> may require explicit parenthesization in or-patterns. Should be easy.

-- Extension idea 5: "This is a useless clause: [...]" trivial except for or-patterns which are difficult I think
