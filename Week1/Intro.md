# [Functional Programming First Lecture](https://en.wikipedia.org/wiki/Functional_programming)

**History**
- David Hilbert - There should be a way to verify mathematical statements that are true.
- Kurt Godel - Epic paradox//Incompletenes theorem states that there are some statements in Mathematics that cannot be proved. E.g. "This statement is not provable". 


**Lambda calculus, Recursion, and turing machines**
- Lambda Calculus is a formal system in mathematical logic for expressing computation based on function abstraction and application using variable binding and substitution. It is a universal model of computation that can be used to simulate any Turing machine.

- A recursive function is a function that calls itself during its execution. The process may repeat several times, outputting the result and the end of each iteration.

- A Turing machine is a mathematical model of computation that defines an abstract machine, which manipulates symbols on a strip of tape according to a table of rules. Despite the model's simplicity, given any computer algorithm, a Turing machine capable of simulating that algorithm's logic can be constructed.


Any recursive algorithm can be a lambda calculus algorithm and both can be represented as Turing machines.


**Imperative vs Functional**

#### Regular
| Imperative | Functional |
| ------ | --- | 
|Mutation of state|Reduction of expressions   |
|Express *how* you want to do something|Express *what* you want to do| 
|Executed in order|Executed in arbitrary order|
|Iteration with loops|Recursion|

E..g in imperative langs:
```java
int i = 1;
for (i = 1; i <= 4; i++) {
  i *= i;
}
```
functional:
```haskell
product[1..4]
```
in actual syntax:
```
product [n] = n
product (n:ns) = n & product ns
```

**Some Functional Programming languages**
- OCaml
- F#
- standardML
- Haskell
- Swift
- Scala (JVM ecosystem)

# Uses 
Anything that can be done in general-purpose programming languages, can be done with functional languages too.

Specifically...
- [Web services](https://medium.com/@nikhilbarthwal/building-web-services-in-functional-paradigm-569ca406ab5) - 
Very closely models how web services work. Events (whether user generated or calls from other services) and Responses can be very easily modeled with ADT. Data immutability captures the behavior of these events, since once an event is generated, it does not change or mutate. A service can be thought of as a function that accepts an event (input to a function) and gives back a response (output of a function). A service may call other services, which is equivalent to a function calling other functions. You can create a system using a set of services, which is essentially the same as creating a higher-order function from other functions.

**[Domain-specific languages](https://en.wikipedia.org/wiki/Domain-specific_language)**

- [Games](https://github.com/Andrea/FunctionalProgrammingInGames)
- [Music](https://www.youtube.com/watch?v=v0HIkFR1EN4)










