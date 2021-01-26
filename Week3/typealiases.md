
# Data Types and Type Aliases

How to define abstract/own types and how built in types are defined.

**Type aliases and data types**
Making custom data types is done through the `data` keyword.
E.g. the data type definition of `Bool` can be visualized as so:

```hs
data Bool = False | True
-- such that these values are contructed and used as so:
True :: Bool
False :: Bool
```

Creating some custom types:

```hs
data Module = CS141 | CS256 | CS132
data Language = Java | JS | Python | Haskell
data Void
```
---
**Pattern matching custom constructors**
Create a type to construct shapes:
```hs
data Shape = Circle Float Float Float | Rectangle Float Float

-- type of Circle and Rectangle
Circle :: Float -> Float -> Flot -> Shape
Rectangle :: Float -> Float -> Shape

-- take a Shape type and return its area: pattern matching the parameters
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle l w) = l * w

```
This means that given 3 float parameters to `Circle` a type `Shape` will be created; same for `Rectangle`.
`area` takes a certain `Shape` type and returns a float value of its area. We can define instances of area such that it can take certain shapes, and through pattern matching will apply the specific formula to calculate the area of that shape.

Define a Point type and update the Shape type's Circle then print the circle's equation:

```hs
data Point = Point Double Double deriving (Show)
data Shape = Rect Double Double | Circle Point Double deriving (Show)


circleequation :: Shape -> [Char]
circleequation (Circle (Point x y) r) = "(x-" ++ show x ++ ")^2 + (y-" ++ show y ++ ")^2 = " ++ show r ++ "^2"
```

Importing types into modules:

```hs
module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where
```

`Shape(..)` will export all constructors for Shape hence any module to import this one will have access to all constructors of the type Shape.

Failing to explicitly state that any constructor of a data type could be imported will result in giving the importer the ability to make shapes only through the auxiliary functions.

Not exporting the value constructors of a data types makes them more abstract in such a way that
we hide their implementation. Also, whoever uses our module can't pattern match against the
value constructors.

---

**Record Types**

Write a record structure such that it is possible to store collective information about a person:

```hs
data Person = Person { firstName :: String
, lastName :: String
, age :: Int
, height :: Float
, phoneNumber :: String
, flavor :: String
} deriving (Show)
```

Instead of:

`data Person = Person String String Int Float String String deriving (Show)`

The main benefit of this is that it creates functions that lookup fields in the data type. By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height, phoneNumber and flavor.

E.g. 
```hs
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
ghci> Car {company="Ford", model="Mustang", year=1967}
```


**Parameterized types**
Type constructors can take types as parameters to produce new types.

The `Maybe` keyword is an algebraic data type.

E.g.
```hs
data MaybeInt = Nothing | Just Int
```

means that `MaybeInt` type returns either nothing or just an integer.

Avoid division by zero with error checking at runtime:

```hs
safeDiv :: Int -> Int -> MaybeInt
safeDiv x 0 = Nothing
safeDiv x y = Just (div x y)
```
We usually use type parameters when the type that's contained inside the data type's various value constructors isn't really that important for the type to work.

Don't put type constraints into data declarations even if it seems to make sense, because you'll have to put them into the function type declarations either way.

Maybe types with parametric polymorphism 

```hs
data Maybe a = Nothing | Just a
```

---

E.g. A vector type and 3 common operations 

```hs
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
```

If you examine the type declaration for these functions, you'll see that they can operate only on vectors of the same type and the numbers involved must also be of the type that is contained in the vectors. Notice that we didn't put a Num class constraint in the data declaration, because we'd have to repeat it in the functions anyway.

It's very important to distinguish between the type constructor and the value constructor. When declaring a data type, the part before the = is the type constructor and the constructors after it (possibly separated by |'s) are value constructors. Giving a function a type of Vector t t t -> Vector t t t -> t would be wrong, because we have to put types in type declaration and the vector type constructor takes only one parameter, whereas the value constructor takes three.

Hence when we declare a custom type `Vector` we give it a single type constructor `a` as such `Vector a = ..` and then the value contructors specify the specific parameters the Vector type will take as so: `data Vector a = Vector a a a`.


---
**Derived Instances**

Instances of type classes can be derived to allow a record to implement its operations and features. 
With the `deriving` keyword it is possible to implement these type classes for our own custom data types.

E.g. compare two `Person`s to see if they are the same people:

```hs
data Person = Person { firstName :: String
, lastName :: String
, age :: Int
} deriving (Eq)

```

This allows the type `Person` to be operated on with `==` and `/=` operations.
There's only one catch though, the types of all the fields also have to be part of the Eq typeclass. 

---

The Enum typeclass is for things that have predecessors and successors.
We can also make it part of the Bounded typeclass, which is for things that have a lowest possible value and highest possible value.


E.g. days of the week can be represented as an Enum type:

```hs
data Day = Monday | Tuesday | Wednesday
| Thursday | Friday | Saturday | Sunday
deriving (Eq, Ord, Show, Read, Bounded, Enum)
```


As the type Enum it is possible to use such functions:

```hs
ghci> succ Monday
Tuesday
ghci> pred Saturday
Friday
ghci> [Thursday .. Sunday]
[Thursday,Friday,Saturday,Sunday]
ghci> [minBound .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
```

---

**Type Synonyms**

The `type` keyword just makes a synonym of an existing type.

E.g. 
`type String = [Char]`




