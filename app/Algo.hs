module Algo where

import Parser
import Data.List (nub)

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
                              in case isComplete s of 

                                -- 2.(a) Case wildcard and sigma is complete: U(P, q) <-> \Or_{k=1}^z useful(S(c_k, P), S(c_k, q))
                                True -> or [useful dts (specializedP c_k (getArity dts c_k) p) (specializedV c_k (getArity dts c_k) q) | c_k <- s]

                                -- 2.(b). Case wildcard and sigma is not complete: U(P, (_ q_2 ... q_n)) = U(D(P), (q_2 ... q_n))
                                False -> useful dts (defaultP p) qs

-- 3. Case or-pattern (r_1 | r_2): U(P, (r_1 | r_2) q_2 ... q_n) = U(P, r_1 q_2 ... q_n) \or U(P, r_2 q_2 ... q_n)
useful dts p q@((POr r_1 r_2): qs) = or [useful dts p (r_1:qs), useful dts p (r_2:qs)]



-- The specialized matrix S(c,P) for constructor patterns. Recursive! The arity of the constructor is needed for the wildcard case.
specializedP :: Constructor -> Int -> PMat -> PMat
specializedP c _ [] = []
specializedP c a (v:vs) = case v of

    -- 1. Case or-pattern (needs to be handled separately, as it produces two rows)
    ((POr r1 r2):ps) -> (specializedP c a [(r1:ps), (r2:ps)]) ++ (specializedP c a vs)

    -- 2. Every other case gets relegated to the vector version of this function
    _ -> (specializedV c a v) : (specializedP c a vs)
specializedP _ _ _ = []


-- The specialized vector S(c,v) for constructor patterns. It needs a for the wildcard case.
specializedV :: Constructor -> Int -> PVec -> PVec 
specializedV c a v = case v of

    -- 1. Case row is constructor pattern 
    ((PCon c' rs):ps) -> if c == c' 
                        then (rs ++ ps) -- If the row's con pattern is the same as the 'original' con pattern
                        else []         -- If it isn't

    -- 2. Case row is wildcard (in our case a var)
    (pv@(PVar v):ps)    -> (replicate a pv ++ ps)
specializedV _ _ _ = []


defaultP :: PMat -> PMat
defaultP = undefined

getSigma :: PMat -> [Constructor]
getSigma xs = nub $ getCons xs
    where getCons (x:xs) = case head x of 
             (PCon c ts) -> c : (getCons xs)
             (POr r1 r2) -> (getCons [[r1], [r2]]) ++ (getCons xs)
             _ -> []
          getCons [] = []

getArity :: DTypes -> Constructor -> Int
getArity = undefined

isComplete :: [Constructor] -> Bool
isComplete = undefined



-- Extension idea 1: Variables instead of Wildcards. Requires Free & Bound defs 

-- Extension idea 2: Desugaring partial application: Allow "((succ succ) zero)" instead of just "(succ (succ zero))" 
--     -> maybe not a good idea.

-- Extension idea 3: "Maybe you forgot this case: [...]?"

-- Extension idea 4: Desugaring alternative constructors: Allow "(_ zero)" which desugars to "zero | (suc zero)",
--          allow "(succ | pred zero)" -> may require explicit parenthesization in or patterns.
