module Algo where

import Parser
-- Implementation of part of "Warnings for pattern matching" by Luc Maranget


-- The useful clause function, defined by:
-- Let P be a pattern matrix of size m × n and let q be a pattern vector of size n.
-- Vector q is useful with respect to matrix P, if and only if:
--  ∃v, P 'doesn't match' v ∧ q 'matches' v.


useful :: PMat -> PVec -> Bool

-- The base cases of the induction, where n = 0
useful _ [] = False -- U((), ()) = False
useful [] _ = True -- U({}, q) = True

-- Cases for n > 0:

-- 1. Case constructed pattern c(p1, p2, ...): U(P, q) <-> U(S(c, P), S(c, q))
useful p q@(c@(PCon con rs): qs) = useful (specializedP c p) (specializedV c q)

-- 2. Case wildcard _:
useful p q@(v@(PVar var): qs) = let s = getSigma p 
                              in case isComplete s of 

                                -- 2.(a) Case wildcard and sigma is complete: U(P, q) <-> \Or_{k=1}^z useful(S(c_k, P), S(c_k, q))
                                True -> or [useful (specializedP c_k p) (specializedV c_k q) | c_k <- s]

                                -- 2.(b). Case wildcard and sigma is not complete: U(P, (_ q_2 ... q_n)) = U(D(P), (q_2 ... q_n))
                                False -> useful (defaultP p) qs

-- 3. Case or-pattern (r_1 | r_2): U(P, (r_1 | r_2) q_2 ... q_n) = U(P, r_1 q_2 ... q_n) \or U(P, r_2 q_2 ... q_n)
useful p q@((POr r_1 r_2): qs) = or [useful p (r_1:qs), useful p (r_2:qs)]



specializedP :: Pattern -> PMat -> PMat
specializedP = undefined

specializedV :: Pattern -> PVec -> PVec 
specializedV = undefined

defaultP :: PMat -> PMat
defaultP = undefined

getSigma :: PMat -> [Pattern]
getSigma = undefined

isComplete :: [Pattern] -> Bool
isComplete = undefined

-- Helper function that allows infix notation that makes sense
isUsefulTo :: PVec -> PMat -> Bool
q `isUsefulTo` p = useful p q



-- Extension idea 1: Variables instead of Wildcards. Requires Free & Bound defs 

-- Extension idea 2: Desugaring partial application: Allow "((succ succ) zero)" instead of just "(succ (succ zero))" 
--     -> maybe not a good idea.

-- Extension idea 3: "Maybe you forgot this case: [...]?"

-- Extension idea 4: Desugaring alternative constructors: Allow "(_ zero)" which desugars to "zero | (suc zero)",
--          allow "(succ | pred zero)" -> may require explicit parenthesization in or patterns.
