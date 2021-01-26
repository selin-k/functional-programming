# Type Classes

Haskell has a static type system. The type of every expression is known at compile time --> safer code. This is because Haskell has type inferencing this means it can infer types without them being explicitly specified.

The `:t` command can be used to check types of functions.

A typeclass is a sort of interface that defines some behavior. If a type
is a part of a typeclass, that means that it supports and implements the
behavior the typeclass describes.

**Ad-hoc polymorphism**
Ad-hoc polymorphism is a kind of polymorphism in which polymorphic functions can be applied to arguments of different types, because a polymorphic function can denote a number of distinct and potentially heterogeneous implementations depending on the type of argument (s) to which it is applied.

E.g. the `+` function in Haskell:

```hs
(+) :: Num a => a -> a -> a
```

Num is a type class and this means that a must be a type that is an *instance of Num*.
For a type to satisfy a type class constraint, it must implement the interface that is described by the type class.
E.g. for some type `a`, constrained by a certain type class constraint, gives us the knowledge of what functions and operations that type will support if it is instantiated.

# Built-in type classes

**Type class constraints**

Type class constraints are used to constrain type variables to only types which support the functions or opertors specified by the type class.

E.g. Num is a type class.. represents types that support arithmetic operators such as +, -, *.

```hs
class Num a where 
  (+) :: a --> a --> a
  (-) :: a --> a --> a
  abs :: a --> a
```

Specifies some method typings in the type class Num ^


**Type class instances**

E.g. Show type class

```hs
class Show a where
  show :: a --> String
```

So a Bool instance of Show would be:

```hs
class Show Bool where
  show True  --> "True"
  show False --> "False"
```

Type errors: E..g if you try to use types that have no instances for certain type classes for example: `Num Bool`

There is no instance of `Bool` for the type class `Num` hence this is a type error if we try to pass a Bool to a function that expects a type that is an instance of the type class `Num` then the compiler will complain.

E.g.

```hs
True + True 

-- won't work since there is no instance of Bool for Num...
```

**Standard library type classes**

- Num for arithmetic operations 
- Eq for equality operators
- Ord for inequality operators
- Show converting things to String
- ...

REPL uses show on the interactive output.

`:i` shows definition and instances of type classes you ask for.



---

Type classes may be placed on arbitrary functions.

```hs
double :: Num a => a -> a
double x = x * 2 -- works for ints/floats/...
```

Types of polymorphism:

*Recap:* In Java there is two main types:
- parametric polymorphism through the use of generics
- subtype polymorphism through the use of subclasses; e.g. where a subclass extends a main class (abstract) - where the main class is expected you could pass an instane of the subclass.

*Haskell polymorphism:*

- parametric polymorphism where type variables could take any type through type inferencing capabilities.
- Ad-hoc polymorphism - a function that accepts types of instances of type classes are open to adding of more instances to that type class such that the original function will still work and change behavior as specified when used.


++ *subtype polymorphism* -

Used to overload functions...


**Superclass Constraints**

The `Ord` type class:

```hs
class Eq a => Ord a where
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (<=) x y = x < y | x = y 
  --  ^ default method implementation hence the dev doesn't have to everytime ord is used...
```

`Eq a =>` is a superclass constraint.
If a is an instance of Ord it must also be an instance of Eq.

---
# Example script

```hs
main :: IO ()


-- can create abstract instances of type classes
instance Num Bool where
    True + True = True 
    _ + _ = False


-- only accepts Int type
addFour :: Int -> Int -> Int -> Int -> Int
addFour x y z w = x + y + z + w

-- a is constraint on the type class Num and can accept
-- Int, Float, Double, etc... check :i Num
addThree :: Num a => a -> a -> a
addThree x y = x + y

```

Abstractions are often represented by type classes. Two examples of this we consider for the following exercises are algebraic structures known as semigroups and monoids.

**Semigroups**
---
A semigroup is any type which has an associative, binary operation. We can define a type class for this where the binary operator is called <>.
A binary operation is a function that takes two arguments. The binary operation must be closed – that is, its two arguments and its return value but must all be values from the same set. A semigroup must also obey one law: the law of associativity.

`x <> (y <> z) == (x <> y) <> z`

Haskell's type class mechanism only allows one instance of each type class for each type.

**Monoids**



