# Syntax

**Pattern matching** consists of specifying
patterns to which some data should conform and then checking to see if
it does and deconstructing the data according to those patterns.

In pattern matching, it's important to have catch cases:

```hs
-- pattern matching
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName x = "Exception (enter a..c)"
```

E.g. here the last case with the x will catch and display an error message.
This avoids the `Non-exhaustive patterns in function' error with the catch-all case.

---
*Exercise: write a recursive factorial function.*
```hs
factorial :: (Integral a) => a -> a
factorial 0 = 1 -- base case
factorial n = n * factorial (n - 1) -- recursive case
```

This reduces as so:

```hs
>> factorial 3

=> 3 * (factorial 2)
=> 3 * (2 * (factorial 1))
=> 3 * (2 * (1 * (1)))
=> 6
```

*Print the first 10 factorials.*
```hs
take 10 [factorial n | n <- [0..20]]
```

The above factorial function is an example of recursion in Haskell.

---

Pattern matching tuples:

```hs
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
```

The type already catches all cases.

Pattern match tuples with list comprehensions:
*Get the sum of pairs/tuples in a list.*
```hs
let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
[a+b | (a,b) <- xs]
==> [4,7,6,8,11,4]
```

When a pattern match fails in list comprehension, the generator moves on to the next iteration.

---

**The @ operator for patterns**

A pattern can be refernced with a cons and the entire refernce can be kept with an identifier as so:

`identifier@(x:xs)`


**Guards**
These are similar to if and else statements, used with pattern matching a lot.

```hs
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

```

Guards are indicated by pipes that follow a function's name and its parameters. Usually, they're indented a bit to the right and lined up. A guard is basically a boolean expression. If it evaluates to True, then the corresponding function body is used. If it evaluates to False, checking drops
through to the next guard and so on.

Instead this function could calculate the BMI with a weight and a height passed to it:

```hs
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
```

However, it would be great if we just calculated it once and used it throughout when checking the pattern cases:

```hs
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2

```

The `where` keyword allows the bmi variable to be defined such that it is only visible throughout the function's scope. This won't necessarily pollute the namespace.

E.g.

Given a name print initials:

```hs
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

```

Could also use pattern matching purely in a single line:

```hs
initials :: String -> String -> String
initials (x:xs) (y:ys) = show x ++ ". " ++ show y ++ "."
```

It's a common idiom to make a function and define some helper function in its where clause and then to give those functions helper functions as well, each with its own where clause.

---

**Let in bindings**

Let bindings can bind to variables anywhere and can be seen after the `in` keyword.

As so:

```hs
ghci> 4 * (let a = 9 in a + 1) + 2
42
```
E.g.
Calculate BMIs given a list of pairs of weight and height fields

```hs
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

```

---

**Case expressions**

The syntax for this construct is as so:

```hs
case expression of pattern -> result
pattern -> result
pattern -> result
...
```

E.g.

The `head` function:

```hs
head' xs = case xs of
  [] -> error "No head for empty lists!"
  (x:_) -> x
```

Given a list, describe it:

```hs
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where
        what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list of length " ++ show (length xs) ++ " "
```






