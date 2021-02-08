# Higher order Functions

Haskell functions can take functions as parameters and return functions
as return values. A function that does either of those is called a higher
order function. 

**Curried Functions**

Every function in Haskell officially only takes one parameter. 

```hs
ghci> max 4 5
5
ghci> (max 4) 5
5

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

=> ((multThree x) 5) 9
```


**Partial Application of Functions**
what is it...

```hs
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

ghci> applyTwice (+3) 10
16
```

---

**Sections // partial application**

```hs
(+)   -> (+)
(+ 8) -> (\x -> x + 8)
(4 +) -> (\y -> 4 + y)
```

Works for any binary operator.

Implementation of `zipWith`

It takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements.

```hs
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

```


Imperative programming usually uses stuff like for loops, while loops, setting something to a variable, checking its state, etc. to achieve some behavior and then wrap it around an interface, like a function.

Functional programming uses higher order functions to abstract away common patterns, like examining two lists in pairs and doing something with those pairs or getting a set of solutions and eliminating the ones you don't need.


```hs
-- flip returns a given function with flipped arguments

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


```
---

# Maps and Filters

- Map - takes a function and a list and applies that function to every element in the list, producing a new list.

```hs
-- takes a function of a to b and returns a list of elements of type b
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

```

You've probably noticed that each of these could be achieved with a list comprehension. map (+3) [1,5,3,1,6] is the same as writing `[x+3 | x <- [1,5,3,1,6]]`. However, using map is much more readable for cases where you only apply some function to the elements of a list, especially once you're dealing with maps.


- Filter - is a function that takes a predicate and a list and then returns the list of elements that satisfy the predicate.

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
```

The above means that if we filter a list with a predicate p, then check the predicate against each element and cons that element to the recursive call to the tail of the list otherwise don't.

---

Idea: Write a function that takes a value and a list and returns the number of occurances/duplicates of that value in the list:

```hs
duplicates :: Eq a => a -> [a] -> Int
duplicates n [] = 0
duplicates n xs = length ys
    where ys = filter (==n) xs
```

Filtered quicksort:

```hs
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
    biggerSorted = quicksort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted
```

---

# Curry and Uncurry Functions

```hs
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y
```

We can write an uncurried add function and then curry it:

```hs
uncurriedAdd :: (Int, Int) -> Int
uncurriedAdd (x,y) = x + y

curriedAdd :: Int -> Int -> Int
curriedAdd = curry uncurriedAdd
```

Where is this useful??

```hs
addPairs :: [Int]
addPairs = map (uncurry (+)) [(1,2), (4,8)]
```

Adding a list of pairs with map.

---

More higher order functions:

- The takeWhile function: takes a predicate and a list and then goes from the beginning of the list and returns its elements while the predicate holds true.

```hs

```


# Lambdas:

Lambdas are basically anonymous functions that are used because we need some functions only once. These are often seen instead of where clauses and need to be wrapped in brackets to determine scope. If pattern matching with lambda expressions fail there will be a runtime error.


E.g.

```hs
ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
[3,8,9,8,7]

```

---

# Folding

Folding is sort of like the map function, only they reduce the list to some single value.

A fold takes a binary function, a starting value/accumulator and a list to fold up. The binary function itself takes two parameters.

The binary function is called with the accumulator and the first (or last) element and produces a new accumulator. Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on.

- `foldl` - left fold 

Implementation of `sum` with `foldl`:

````hs
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
``````

where the lambda clause `(\acc x -> acc + x)` is the binary function and 0 is the starting value.

Except: The lambda function (\acc x -> acc + x) is the same as (+).

Hence:

```hs
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

```
also does the job.

Generally, if you have a function like foo a = bar b a, you can rewrite it as foo = bar b, because of currying.

- `foldr` right fold

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

and' :: [Bool] -> Bool
and' = foldr (&&) True

product' :: Num a => [a] -> a
product' = foldr (*) 1

length' :: [a] -> Int
length' = foldr (\x n -> n + 1) 0
```

---

**Using `foldl` vs `foldr`**

- `foldl`

```hs
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
```

- `foldr`

```hs
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
```

Notice that the accumulator eats from the side that fold folds from. 

Folding from the right is more common when building up new lists from lists. This is because working from bottom up allows prepending with `:` instead of using a `++` if we were to left fold. Otherwise, left folds would reverse the list. 

`++` is more expensive than `:`.


One big difference is that right folds work on infinite lists, whereas left ones don't! To put it plainly, if you take an infinite list at some point and you fold it up from the right, you'll eventually reach the beginning of the list. However, if you take an infinite list at a point and you try to fold it up from the left, you'll never reach an end.


---

**`foldl1` and `foldr1`**

The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value.

They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it.

These versions depend on the list passed having a value. Hence, an empty list would cause runtime errors while `foldl` and r wouldn't due to the explicit start value provided.





