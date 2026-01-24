# pattern-matching-exhaustiveness-and-redundancy-checker

---

A reusable library to check the correctness and exhaustiveness of nested pattern matching data types, defined as ADTs (Algorithmic Data Types).

---

#### The instructions:

- Define set of ADTs with constructors and field types
- Receives type of initial expression + set of branches (ie. patterns) as inputs
- If exhaustive + correct: return which variables of which type are bound in branch #1, #2, etc.
- Failure cases: type mismatch, not exhaustive, unreachable branches
- Possible extension points
  - more complicated patterns (eg. or patterns, `Some(Pair(suc x, _) | Pair(_, suc x))`)
  - reasonable representation/printing of non-exhaustive patterns
  - lazy pattern matching

---

#### Code structure:

- src
  - reader
  - parser
  - main
  - target
  - test

- resources
  - input
  - test_input
- lib



##### Workflow

input -> reader -> parser -> main -> target

##### Test workflow

test_input -> reader -> parser -> test -> main -> target