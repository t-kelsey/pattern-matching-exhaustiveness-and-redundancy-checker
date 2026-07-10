# pattern-matching-exhaustiveness-and-redundancy-checker

---

A reusable library to check the correctness and exhaustiveness of nested pattern matching data types, defined as ADTs (Algorithmic Data Types).

---

## The instructions given for the project

- Define set of ADTs with constructors and field types

- Receives type of initial expression + set of cases (ie. patterns) as inputs

- If exhaustive + correct: return which variables of which type are bound in case #1, #2, etc.

- Failure cases: type mismatch, not exhaustive, unreachable cases

- Possible extension points
  - more complicated patterns (eg. or patterns, `Some(Pair(suc x, _) | Pair(_, suc x))`)
  
  - reasonable representation/printing of non-exhaustive patterns
  
  - lazy pattern matching
  
    


## Usage
---
### DEFINING YOUR INPUT

---

In resources/input.txt, define your data types and pattern matrix with haskell-like syntax:

##### Data types

```hs
=== data types ===
data ExampleType where
  exampleConstructor : InputType1 -> InputType2 -> ExampleType
```
Remember the "=== data types ===" at the beginning! It is used as a separator.

##### Pattern Matrix

```hs
=== pattern matrix ===
(exampleConstructor1 | exampleConstructor2)     exampleConstructor3
           exampleConstructor3                           x
```

Spaces are counted as separators in the matrix: The centering here is just for readability. Newlines are new rows in the matrix.

Or-patterns have (limited) implicit grouping, so parenthesis are usually not needed.

You can use comments the the parser will ignore at the top of the input.

Unlike haskell, I've defined constructors to be allowed to be lower case (to better differentiate them from types), and instead of '::' the bindings is declared after the ':'. Of course, the syntax is decoupled from the language and simple changed in the parser can allow you to read any format you wish.

---

### RUNNING ON YOUR INPUT

---

`cabal run`
Reads your input, then outputs the result:    _'resources/input.txt' -> Parser.hs -> Main.hs_

`cabal test`
Runs the full test suite:    _'test/integration/*.txt -> Parser.hs -> resources/Test.hs -> Main.hs_

---

## Help! What does this all mean?

---

In functional programming, it is of use to the compiler to guarantee that functions are *total*, that means that for all inputs, an output is defined of the expected type.

This is important as *total* functions may be reduced and such your code be optimized, while *partial* functions may crash at runtime. Given this example:
`timesTwo ( dividedByTwo ( x ))` -> we can see the result is always equal to `x` if the functions are *total*, and we can simplify the function composition to `x`!

Imagine though that `timesTwo` *isn't* total: `timesTwo = \x -> if x == 1 then 2, else if x == 2 then 4, else if x == 3 then 6,` 
-> `timesTwo(4)` isn't defined! So we would **change** the output of the program if we were to simplify the function composition. 

Hence when defining a function, the make sure that **all** possible inputs of the correct type are accounted for, else we give the programmer a warning: "Hey, you're missing a case!"

A pattern matrix is then in fact just a bunch of cases we check the input on:
```hs
if x == True then do a
else if x == False then do b
```
That turns into the pattern matrix (remember that we're just checking that all possibilities are present, so we can ignore the 'do a' and 'do b')
```hs
=== pattern matrix ===
True
False
```

Now as the big step to get a matrix and not a vector, remember that functions can have multiple arguments (even if the pretend they don't @haskell)
```hs
define function 'and' x y as
	if x == True AND y == True then return True
	else if x == True AND y == False then return False
	else if x == False AND y == False then return False
	else if x == False AND y == True then return False
```
That doesn't seem efficient! Can't we have a catch-all case or something so we don't need to write every possible combination?
```hs
define function 'and' x y as
	if x == True AND y == True then return True
	else if x is whatever AND y is whatever then return False
```

Now, convert it to pattern matrix representation:
```hs
=== pattern matrix ===
True True
x    y 
```

Here `x` and `y` are like wildcards: We don't care what they are, but we can use them in our `do a` to be able to use them in future computations!

Finally, realize that constructors can take arguments themselves which we can match too! So instead of the nullary constructor `True` of type `Bool`, we can match constructors with any arity! If we define a constructor `boolCons : Bool -> List -> List` we can match complicated values such as `(boolCons True [])` ([] is also a constructor of `List`).

---
## Code structure

---

- app
  - Parser               -- Parses the input and checks returns error messages for malformed inputs
  - Main                  -- Bundles parsing, compilation, execution, and terminal output into an entry point
  - UsefulClause    -- Handles the central function and helper functions of the algorithm
  - Detection           -- Detects compilation errors and further debug info, such as "this case is redundant"
- resources
  - input                  -- Where to define your input
  - example            -- An example input
- test
  - Test.hs              -- The test suite runner
  - integration
    - ...                -- The individual tests in .txt format


---

## Most important functions of library

`useful :: DTypes -> PMat -> PVec -> Bool`
Check if a given value vector is useful to a pattern matrix, i.e. if there are additional
cases not covered by the pattern matrix that are covered by the value vector.

`exhaustive :: DTypes -> PMat -> Bool`
Check is a pattern matrix is exhaustive under defined data types.
$P$ is exhaustive if and only if $\text{useful}(P,\_ ... \_)$ is false.

`containsUselessRow :: DTypes -> PMat -> Maybe PVec`
Check if the given pattern matrix contains a useless row.
$P$ does not have useless rows if $\text{uselessrow}(P, i)$ is false for all rows $i$.
Always just gives the last useless row, but all information is there:
Once the last row is corrected, the useless row infront is detected.

`warnings :: DTypes -> PMat -> String`
This is the main function of this project. The output is the combination of warnings 
generated, and of course the exhaustiveness check result, along with semantic checking of the input.

---

