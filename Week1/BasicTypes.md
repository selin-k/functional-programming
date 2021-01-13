# Basic Types

Functional programs are expressed in the form of functions. Variables cannot actually vary; hence, a function 'has no side-effects'. 
Functions have:
- referential transparency : allows the compiler to reason about programs and it is easy to deduce and prove the truth of functions.
- lazy : means that unless told not to, Haskell will not execute any functions until it is forced to show a result.
- statically typed : means when you compile a program, the compiler knows what is a string or a number and so on. Hence, the compiler catches a lot of possible errors at compile time. Haskell uses ' type inference' which means that types don't have to be explicitly labeled in code since the type system can figure it out.


**Basic Types**
`expression :: type`
is the syntax to indicate that an expression has a certain type.
Once we assign a type to an expression, then that expression is called 'well typed'.

E.g.
```hs
42 :: Int
True :: Bool
'c' :: Char
"Minecraft" :: String
0.5 :: Double
```

*A type is an approximation of the value of an expression.*

Typings can be done as so, especially in documentation:
```hs
t0 :: Int
t0 = 42

t1 :: Bool
t1 = True

t2 :: Bool
t2 = False

t3 :: Char
t3 = 'c'

-- and so on...
```

**Function Types**
`not Bool -> Bool`
Defines the type of the function `not`. This means a `Bool` value is passed in and a `Bool` value is passed out.

```hs
ourNot :: Bool -> Bool
ourNot = \b -> case b of
    True  -> False
    False -> True
```

**Paramterized types : Tuples**
Tuples are a finite ordered list of elements. An n-tuple is a sequence of n elements, where n is a non-negative integer.

Can be typed as:
```hs
(4, 7) :: (Int, Int)
(5, 0.5) :: (Int, Double)
('a', 9, "Hello") :: (Char, Int, String)
((4, 'g'), False) :: ((Int, Char), Bool)
```

Functions involving pairs:
`fst :: (a,b) -> a` given a pair returns the first element.
`snd :: (a,b) -> b` given a pair returns the second element.
`swap :: (a,b) -> (b,a)` swaps elements of a pair.


**Parametric Polymorphism**

The type of `\x -> x` is a permissible type such that x will just return the same type it is assigned, as spotted y the type checker.
This can also be expressed as:
`\x -> x :: a -> a`
Therefore something like:
`f True` will be assigned type `Bool -> Bool` by the type checker at compile-time.










