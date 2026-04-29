module Algo where

import Parser
-- Implementation of part of "Warnings for pattern matching" by Luc Maranget


-- The useful clause function, defined by:
-- Let P be a pattern matrix of size m × n and let q be a pattern vector of size n.
-- Vector q is useful with respect to matrix P, if and only if:
--  ∃v, P 'doesn't match' v ∧ q 'matches' v.


useful :: Matrix -> Vector -> Bool

-- The base cases of the induction, where n = 0
useful _ [] = False -- U((), ()) = False
useful [] _ = True -- U({}, q) = True

-- Cases for n > 0:

-- 1. Case constructed pattern c(p1, p2, ...): U(P, q) <-> U(S(c, P), S(c, q))
useful P q@(c@(PCon con ps): qs) = useful (specializedP c P) (specializedV c q)

-- 2. Case wildcard _:
useful P q@(v@(PVar var): qs) = let s = getSigma P 
                              in case isComplete s of 

                                -- 2.(a) Case wildcard and sigma is complete: U(P, q) <-> \Or_{k=1}^z useful(S(c_k, P), S(c_k, q))
                                True -> or [useful (specializedP c_k P) (specializedV c_k q) | c_k <- s]

                                -- 2.(b). Case wildcard and sigma is not complete: U(P, (_ q_2 ... q_n)) = U(D(P), (q_2 ... q_n))
                                False -> useful (defaultP P) qs

-- 3. Case or-pattern (r_1 | r_2): U(P, (r_1 | r_2) q_2 ... q_n) = U(P, r_1 q_2 ... q_n) \or U(P, r_2 q_2 ... q_n)
useful P q@((POr r_1 r_2): qs) = or [useful P (r_1:qs), useful P (r_2:qs)]


-- Helper function that allows infix notation that makes sense
isUsefulTo :: Vector -> Matrix -> Bool
q `isUsefulTo` P = useful P q