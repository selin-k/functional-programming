# Lists

**Constructing lists**
Lists can be used as expressions. Lists are **homogenous** data structures, tis means it stores several elements of the same type. 
Strings are lists of characters.

**Some list functions in the base package**
- Concatenation of lists: `[1,2,3,4] ++ [9,10,11,12]`
- prepend : ``
- tail : `tail :: [a] --> [a]` returns the rest of the list minus the first element.
- init : `init [a] -> [a]` returns everything in a list except the last element.
- head : `head :: [a] --> a` returns the first element from a list.
- take x : `take :: Int --> [a] --> a`
- drop x : `drop Int --> [a] --> [a]` drops int number of elements from the beginning of a list.
- null : `null :: [a] --> Bool`
- length : `length :: [a] --> Int`
- (++) : `++ :: [a] --> [a] --> [a]` append to list
- (:) : `(:) :: a -> [a] -> [a]` inserting to index 0.
- (!!) : `(!!) :: [a] -> Int -> a` returns item at specified index.
- concat : `concat :: [[a]] --> [a]` given a list of lists of as flattens them
- reverse : `reverse :: [a] --> [a]`
- and `and :: [Bool] --> Bool` and operator for more than 2 boolean values
- product: `product :: [Int] --> Int` multiplicative product of a list of integers.
- sum : `sum [a] --> a` 
- replicate : `replicate :: Int --> a --> [a]`
- maximum/minimum : `maximum [a] --> a` 
- elem : `elem a [a] --> Bool` .contains type function

**Pattern-matching on lists**

Determine if a list is empty:
`null[]      = True`
`null (x:xs) = False`

`x:xs` is a 'cons' pattern where we give a head and a tail such that head is `x` and `xs` is the tail.

`x :: a`
`xs :: [a]`

The cons pattern need to be called x and xs per se. These are arbitrary identifiers...

hence:
`head (x:xs) --> x`
`tail (x:xs) --> xs`

List patterns:
```hs
[] -- empty list
(x:xs) -- list of at least one element
(x:y:xs) -- at least two elements
(True:xs) -- at least one element which is True
[x] -- exactly one element
[x,y] -- exactly two elements
```

Lists are syntactic sugar :: lists are made of `cons` which are list/data constructors.

`[1,2,3] = 1 : (2 : (3 : []))`


Lists are of type `[] : [a]` list of 'a'.

`(:) :: a --> [a] --> [a]` hence cons can be used to add to lists from index 0 such that if we take `True` and we cons it to the empty list we get a list of bools as so: `True:[]`...
Then we can use cons patterns to take them away again...

This list would have a type as so: `[True] :: [Bool]` which means a 'list of boolean(s)'.

---

More list functions (abstract):

```
splitAt 2 [1,2,3,4,5]
--> ([1,2] [3,4,5])
```

splitAt returns a tuple of components of an array. This is : `splitAt :: Int --> [a] --> ([a],[a])`

This function can be written as so:

```hs
splitAt n xs = (take n xs, drop n xs)
```

It is possible to write elements as expressions within a list as such:
`[1+2, 4*7, not True]` ...


# Ranges

`[1,3..10]` --> `[1,3,5,7,9]`
`['a', 'd'..'m']` --> `['a','d','g','j','m']`

Infinite lists are possile in Haskell..

**List comprehensives**

`[n | n <-- [0..5]]`

The rigth side is called a generator and the n is esentially a loop guard like in Java and this is pretty much similar to a for loop...

`==> [0,1,2,3,4,5]`

So, we can have useful generations like:

`[even n | n <- [0..5]]`
and
`[n*m | n <- [0..2], m <- [0..2]]`

the generator above would produce all possible combinations of if every value in n was ultiplied by every value in m - so n choose m but n*m... This is a nested foor loop.

It is posile to constrain the length of the second list to the expand as the loop guard changed index:

`[n*m | n <-- [0..2], m <-- [0..n]]`

The left hand side is just a pattern for the generator loop.

# Filtering and pattern matching
Pattern matching with generators:

`[x | (c,x) <-- [('a', 5), ('b', 7)]` means to extract the x, integer, from pairs of c, x.
`==> [5,7]`

`[length xs | x:xs <- [[1,2],[3,4,5]]]` taking the tail of each list and calulating the length...
`==> [1,2]`


*Predicates in list comprehension:*

Predicates go after the binding parts and are separated from them by a comma:

`[n | n <- [0..100], mod n 2 == 0]`

Would print all values in between 0 and 100 that have a remainder of 0 when divided by 2,hence all multiples of 2.

If you want to take the first 10 even numbers:

`take 10 [n | n <- [0..100], n 'mod' 2 == 0]`

List comprehensions can also be used to generate lists from other lists, just like set
comprehensions are used to generate sets from other sets. For example, the following
expression is a list comprehension which generates the list of numbers that are double
the numbers from 0 to 10:

`2*n | n <- [0..10]`
The `n <- [0..10]` part in this example is referred to as a generator. Given some
list on the right of `<-`, it loops through all the elements of that list and binds them
to n one after the other. The part on the left of the | is what is used to generate an
element of the resulting list, for all values obtained from the right of the |.


**Function for length of lists**

```hs
length :: Num a => [t] -> a
length n = sum [1 | _ <- n]
```

Replaces anything within the list n with 1 and sums it up.


