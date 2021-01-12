# Definitions & Functions
**How do we write programs in Haskell?**
Haskell is a standardizes language, most recent one is 2010.
GHC is a popular implementation of it. It uses many extensions on top of Haskell 2010.
 
**Compiling**
Plaintext source files with .hs extensions are executed with the GHC compiler to produce an executable object code.
During compilation, the source code goes through the parser, type-checking, and gode gen.. Haskell is statically typed hence type-checking is performed during compilation.
There are two main compiler errors:
- Parse errors: Syntax errors.
- Type errors

**Haskell Modules - sourcefiles**
E.g. a file called Foo.hs
```hs
module Foo where 
-- name the modules after the filename...
{- this is a multiple line comment
  -}
```
P.S. it is possible to write nested multi-line comments.
- Without an explicitly defines module name GHC assumes "Main".

**Definitions**
Definitions are simply written as:
```
name = expression
```
The identifier name is always starts with a lower-case character.
E.g.
```hs
daysoftheweek = 7
ex1 = "Hello World"
ex2 = 'c'
ex3 = True
ex4 = False
```
Similar to python sort of...

**Arithmetic Expressions/Operators**
```hs
...
hoursperweek = daysperweek * 24
4 + 8
15 - 16
23 * 24
True && False
"abcdef" !! 0 -- indexing 
"CS" ++ "141 -- concat
("Hello" ++ " " ++ "World") !! 5 -- concat a string ang get index 5??
```
All of these expressions are simply "reduced" by GHC.

**Sidenote: GHCi**
Interactive coding with `stack ghci` will evaluate and reduce expressions real-time without the need to compile and run a binary file.

*Boolean expressions*
- Negation : `not True` is False. `not (not True)` is True.
- AND: `True && False`
- OR: `True || False`

*Maths*
- f(x) - `f x`
- f(x,y) - `f x y`
- f(g(x)) - `f (g x)`
- f(x)g(x) - `f x * g x`

---

A Haskell Module is a collection of definitions

Definition: `daysInWeeks = \w -> w * 7`
This means that `daysInWeeks` will take a value `w` and return `w * 7`.

Reduction:
```hs
daysInWeeks 2
==> (\w -> w * 7) 2
==> 2 * 7 -- substitute w with 2.
==> 14
```
 *Recap: Lambda Calculus*
 - Function definitions start with a lambda, in Haskell `\` is the lambda keyword.

 For example, In other languages:
 - Python : `lambda w: w * 7`
 - Java : `w -> w * 7`
 - JS : `w => w * 7`

---

Implement an Exclusive OR (XOR) without a built-in function.

Definition: `xor = \a -> \b -> (a || b) && not (a && b)`
Takes in two values a and b as seen above.

Reduction:
```hs
xor True False
==> (\a -> \b -> (a || b) && not (a && b)) True False
==> (True || False) && not (True && False) -- this happens in 2 steps
==> True && not False
==> True && True
==> True
```
The process of applying arguments to a function one by one is called `Currying`, after Haskell Curry.
Currying allows 'partial function application'.
E.g.
```
add = \x -> \y -> x + y
addFive = \x -> add 5 x 
```
- `addFive = add 5` would be the exact same definition...
- `add` is just a `+`

Therefore, finally a reduction would be:
```hs
add = \x -> \y -> x + y
addFive = add 5

add 5 5 
==> 10
addFive 5
==> 10
```
---

**Syntactic Sugar**
Nicer syntax basically...

```hs
daysInWeeks = \w -> w * 7
-- can be written as:
daysInWeeks w = w * 7
```
```hs
xor = \a -> \b -> (a || b) && not (a && b)
-- can be written as:
xor = \a b -> (a || b) && not (a && b)
-- can also be written as:
xor a b = (a || b) && not (a && b)
```

**Pattern Matching**

Define `not` in a function:
```
Not = \x -> case x of
  True -> False
  False-> True
```

Reduction:
```hs
Not True
==> (\x -> case x of
  True -> False
  False-> True) True 
==> case True of
  True -> False
  False-> True
==> False
```

Case expressions are just like switch statements. This expression can also be written as:
```hs
Not True = False
Not False = True
```

Patterns are checked from top to bottom.

**Conditionals**

Print the minimum of two numbers in two ways:
```hs
-- If/else
min x y = if x < y then x else y 
```

```hs
-- guards
min x y | x < y     = x 
        | otherwise = y
```

**Layout/Indentation/Scopes**
*The Layout rule*
- Like in python, Haskell is indentation sensitive - which determines scope.









