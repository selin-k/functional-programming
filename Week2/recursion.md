# Recursive Functions

How do we express loops without mutable state?

What is 'mutable state' referring to?
We can't change state, things are evaluated through reduction of definitions.


**Calculate the nth factorial**

Non-recursive:
```hs
factorial :: Int -> Int
factorial n = product [1..n]
```

Recursive
```hs
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
```

It may not be possible to represent all values of n! with the type `Int`. Instead, `Integer` can be considered as a superset of `Int`. This value is not bounded by any number, hence an `Integer` can be of any length without any limitation.

**Calculate the nth Fibonacci number**
```hs
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
```

**Define the product function in Haskell**
```hs
product :: [Int] -> Int
product [] = 1
product (n:ns) = n * product ns
```

The `n:ns` is the cons pattern.

**splitAt definition in Haskell**

```hs
splitAt :: Int -> [a] -> ([a],[a])
splitAt 0 xs     = ([], xs)
splitAt _ []     = ([], [])
splitAt n (x:xs) = (x:ys, zs)
    where (ys,zs) = splitAt (n-1) xs
```

Another way:

```hs
splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs =
  let ys = take n xs
      zs = drop n xs
  in (ys, zs)
```

```hs
splitAt :: Int -> [a] -> ([a],[a])
splitAt 0 xs     = ([], xs)
splitAt _ []     = ([], [])
splitAt n (x:xs) = 
  let (ys, zs) = splitAt (n-1) xs
  in (x:ys, zs)

-- let binding / let expression 
```

Step by step reduction of splitAt

```hs

splitAt 2 [1,2,3]
=> (1:ys, zs)
  where (ys, zs) = splitAt 1 [2,3]
=> (1:ys, zs)
  where (ys, zs) = (2:ys', zs')
        (ys', zs') = splitAt 0 [3]
=> (1:ys, zs)
  where (ys, zs) = (2:ys',zs')
        (ys',zs') = ([], [3])
=> (1:ys, zs)
  where (ys, zs) = (2:[], [3])
=> (1:2:[], [3])
== ([1,2], [3])
```


---

A couple of demo functions with recursion in Haskell

```hs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

length :: [a] -> Int
length [] = 0
length (x:xs) = length xs + 1

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs 


replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- pattern matching
concat :: [[a]] -> [a]
concat [[]] = []
concat [(x:xs)] = x:xs

-- recursive concat
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss


zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
```


