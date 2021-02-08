# Testing and Config

**Haskell Packages**

- A package is a collection of Haskell modules. Usually most are found on Hackage.
- Packages can depend on certain versions of specific packages.
- Haskell packages are managed by cabal.

A cabal usually configuration looks as so:
```hs
name:                lecture
version:             1.1.0.0

library
  -- directory that contains the .hs files
  hs-source-dirs:      src
  exposed-modules:     Lecture
  -- version constraints of Haskell packages
  build-depends:       base >= 4.7 && < 5
  default-language:    Haskell2010
```
Cabal configurations can be initialized with the `cabal init -i` command.

---
**Stack is a cabal replacement**
Stack uses package snapshots to deal with dependency incompatibilities. These snapshots specify compatible package versions. i.e. `lts-16.27`


---
**Pure vs Impure functions**
| Pure | Impure |
| ------ | --- | 
| Take some input and return some output based on it. | Output is not deterministic and doesn't solely depend on inputs. |
| No side effects | Mya have side effects like network or database calls |
| Inputs are not modified. | Inputs may be modified |

Most languages are impure. Haskell is pure.

Conclusion: Haskell good. Everything else bad.
In other words:
```hs
pure :: String -> Bool
pure language = if language == "Haskell"
```



