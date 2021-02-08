# Higher Order Functions and Evaluation

**Reduction of fold functions**

```hs
foldl f z [1,2,3]
=> f(f(f z 1) 2) 3

foldr f z [1,2,3]
=> f 1(f 2(f 3 z))
```

These basically fold from the left or right, by applying the function iteratively to each element and keeping it as z. then applying the function again with the next element and keeping it in z.

i.e. `map` with `foldr`

```hs
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
```
Reminder: creating new lists from given lists usually uses foldr; allows the usage of `:` over `++`.


**Function Composition**

Finding duplicates with a `foldr` function:

```hs
countfold :: (Foldable t, Eq a1, Num a2) => a1 -> t a1 -> a2
countfold y = foldr (\x acc -> if y==x then 1+acc else acc) 0
```

This job can also be done through recursion or the use of `filter` and `length`.

```hs
-- filter and length
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count n xs = length ys
    where ys = filter (==n) xs

-- better filter and length with (.)
countp :: Eq a => a -> [a] -> Int
countp y = length . filter (==y)

-- recursive count with guards
count' :: Eq a => a -> [a] -> Int
count' _ [] = 0
count' y (x:xs) 
    | y == x    = 1 + count y xs
    | otherwise = count y xs
```

The take away from function composition is `(.)` this function. 


---

# Lazy Evaluation 

**Order of evaluation**
An expression that is fully reduced is in `normal form`.

Usually only case expressions are only evaluated.

An expression that can be reduced is called a `redex`. For example:

```
(2 + 2) * (3 + 1) --> the brackets are redexes
fac 500           --> is a funciton redex
```

**Strictness**

Haskell is a non-strict language. Allows strategies such as:

- Call by name: expressions given to functions as arguments are not reduced before the function call is made. This means redexes are only reduced when needed. May produce the same expression over and over.

VS

- Call by value: reduce the function arguments before calling the function with those arguments. This means redexes may be reduced needlessly.

**Sharing**

Avoid duplicate evaluation, call by name problems are dealt with using let in bindings to store expressions in memory.

```hs
-- fac' with explicit case expression

fac' :: Int -> Int -> Int
fac' n m = case n of
    0 -> m
    _ -> fac' (n-1) (n*m)

--------------------------------------------------------------------------------
-- fac' with explicit let-bound arguments

fac'' :: Int -> Int -> Int
fac'' n m = case n of
    0 -> m
    _ -> let x = n-1
             y = n*m
         in fac'' x y
```

This basically keeps values in memory to avoid re-evaluation. This is similar to dynamic programming techniques, e.g. memoisation.

---

**Therefore, call by name + sharing = lazy evaluation hence a non-strict paradigm without the multiple evaluation of the same expressions.**

---

The 'lazy' compiler takes expressions and produced let bindings on every redex' components and removes syntactic sugar.


Strictness in Haskell

```hs
fac''' :: Int -> Int -> Int
fac''' 0 m = m
fac''' n m = (fac''' $! (n-1)) (n*m)
```

will allow call-by-value, hence strictness.. for whatever reason it is desired...



---

**DEMOS**

```hs
-- Internal representations of map, take, and length generated and
-- used by the compiler

map :: (a -> b) -> [a] -> [b]
map = \f -> \arg -> case arg of
    []     -> []
    (x:xs) -> let y  = f x
                  ys = map f xs
              in y : ys

take :: Int -> [a] -> [a]
take = \n -> \xs -> case n of
    0  -> []
    n' -> case xs of
        []     -> []
        (y:ys) -> let x  = n' - 1
                      zs = take x ys
                  in y : zs

length :: [a] -> Int
length = \xs -> case xs of
    []     -> 0
    (y:ys) -> let n = length ys
              in 1 + n

```









