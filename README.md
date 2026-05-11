# pattern-matching-exhaustiveness-and-redundancy-checker

---

A reusable library to check the correctness and exhaustiveness of nested pattern matching data types, defined as ADTs (Algorithmic Data Types).

---

#### The instructions

- Define set of ADTs with constructors and field types
- Receives type of initial expression + set of branches (ie. patterns) as inputs
- If exhaustive + correct: return which variables of which type are bound in branch #1, #2, etc.
- Failure cases: type mismatch, not exhaustive, unreachable branches
- Possible extension points
  - more complicated patterns (eg. or patterns, `Some(Pair(suc x, _) | Pair(_, suc x))`)
  - reasonable representation/printing of non-exhaustive patterns
  - lazy pattern matching

---

#### Code structure

- src
  - Parser
  - Main
  - UsefulClause
  - Detection

- test
  - Test

- resources
  - input
  - test

---

### Most important functions of library


`useful :: DTypes -> PMat -> PVec -> Bool`\
Check if a given value vector is useful to a pattern matrix, i.e. if there are additional
cases not covered by the pattern matrix that are covered by the value vector.


`exhaustive :: DTypes -> PMat -> Bool`\
Check is a pattern matrix is exhaustive under defined data types.
$P$ is exhaustive if and only if $\text{useful}\(P,\\_ ... \\_\)$ is false.


`containsUselessRow :: DTypes -> PMat -> Maybe PVec`\
Check if the given pattern matrix contains a useless row.
$P$ does not have useless rows if $\text{uselessrow}\(P, i\)$ is false for all $i$.
Always just gives the last useless row, but all information is there:
Once the last row is corrected, the useless row infront is detected.


`warnings :: DTypes -> PMat -> String`\
This is the main function of this project. The output is the combination of warnings 
generated, and of course the exhaustiveness check result, along with semantic checking of the input.

---

##### Workflow
`cabal run`
input -> parser -> main

##### Test workflow
`cabal test`
test_input -> parser -> test
