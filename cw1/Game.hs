--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module Game where 

import Prelude
import Data.List

--------------------------------------------------------------------------------

-- | Represents different actions (including their parameters) that a cell can 
-- have on a row or column total.
data Action 
    = Add Int 
    | Sub Int 
    deriving (Eq, Ord, Show)

-- | Represents a cell including whether it is enabled and its action.
data Cell = MkCell Bool Action
    deriving (Eq, Show)

-- | A row has a target number and consists of zero or more cells.
data Row = MkRow Int [Cell]
    deriving (Eq, Show)

-- | A grid is comprised of the target numbers for all columns and the rows.
data Grid = MkGrid [Int] [Row]
    deriving (Eq, Show)

-- | Enumerates directions in which lists can be rotated.
data Direction = L | R
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | `eval` @action total@ applies @action@ to the running @total@. 
-- For example:
--
-- >>> eval (Add 5) 3
-- 8
--
-- >>> eval (Sub 1) 3
-- 2
--
---------------------------------------
-- The argument of the action and some other argument to be applied to that action are defined to return 
-- the value of the selected arithmetic operation on the two arguments, through pattern matching.
---------------------------------------
eval :: Action -> Int -> Int 
eval (Add n) m = m + n
eval (Sub n) m = m - n

-- | `apply` @cell total@ applies the action of @cell@ to the running @total@ 
-- if @cell@ is enabled. For example:
--
-- >>> apply (MkCell True (Add 5)) 3
-- 8
--
-- >>> apply (MkCell False (Add 5)) 3
-- 3
--
---------------------------------------
-- Given a cell with a state and an action, and a total that the action will be applied to; the action
-- is only evaluated if the state of the cell matches to True. Otherwise, the total is returned.
---------------------------------------
apply :: Cell -> Int -> Int 
apply (MkCell True action) total = eval action total
apply (MkCell False _) total = total

-- | `result` @cells@ calculates the total produced by the actions of all 
-- enabled cells in @cells@ starting from 0. For example:
--
-- >>> result []
-- 0
--
-- >>> result [MkCell True (Add 5), MkCell False (Add 5), MkCell True (Sub 1)]
-- 4
--
---------------------------------------
-- If the list of cells are empty, return 0. Otherwise, iterate over the list and apply
-- each cell to 0 and add to the accumulator.
---------------------------------------
result :: [Cell] -> Int 
result [] = 0
result xs = foldl (\acc x -> acc + apply x 0) 0 xs

-- | `states` @cell@ is a function which returns a list with _exactly_ two
-- elements that represent the two different states @cell@ can be in. For
-- example:
--
-- >>> states (MkCell False (Add 5))
-- [MkCell True (Add 5), MkCell False (Add 5)]
--
---------------------------------------
-- This here uses pattern matching, but i could make use of the given input also,
-- print the given and print the other state along with it...
---------------------------------------
states :: Cell -> [Cell]
states (MkCell _ action) = [cell1, cell2]
    where cell1 = MkCell False action
          cell2 = MkCell True action

-- | `candidates` @cells@ is a function which, given a list of cells in a row,
-- produces all possible combinations of states for those cells. For example:
-- 
-- >>> candidates [MkCell False (Add 5), MkCell False (Sub 1)]
-- [ [MkCell False (Add 5), MkCell False (Sub 1)]
-- , [MkCell False (Add 5), MkCell True (Sub 1)]
-- , [MkCell True (Add 5), MkCell False (Sub 1)]
-- , [MkCell True (Add 5), MkCell True (Sub 1)]
-- ]
--
---------------------------------------
-- REVISE THIS HERE:
-- Given a list of cells, iterates over these cells and for each state of each cell,
-- prepends the elements of the recursive call to candidates.
---------------------------------------
candidates :: [Cell] -> [[Cell]]
candidates [] = [[]]
candidates (x:xs) = [z:y | z <- states x, y <- ys]
    where ys = candidates xs


-- shit version:
--candidates :: [Cell] -> [[Cell]]
--candidates [] = [[]]
--candidates (x:y:xs) = [[a,b]| a <- states x, b <- states y] ++ candidates xs


-- | `solveRow` @row@ finds solutions for @row@. For example:
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Sub 1)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Sub 1)]]
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Add 5)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Add 5)] 
-- , MkRow 5 [MkCell False (Add 5), MkCell True (Add 5)]
-- ]
--
---------------------------------------
-- For a given row, calculate the list of candidates for that row and for each candidate
-- compare the result to the annotated goal of that row. Where the goal is met, return 
-- the candidate list of cells as a row.
---------------------------------------
solveRow :: Row -> [Row]
solveRow (MkRow goal xs) = [MkRow goal y | y <- ys, result y == goal]
    where ys = candidates xs

-- | `solve` @grid@ finds all solutions for @grid@. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> solve (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell True (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell True (Add 2), MkCell True (Add 2)]
--                ]
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
solve :: Grid -> [Grid]
solve = undefined

-- | `rotate` @direction list@ rotates the items in @list@ to the left or
-- right depending on the value of @direction@. For example:
--
-- >>> rotate L [1,2,3]
-- [2,3,1]
--
-- >>> rotate R [1,2,3]
-- [3,1,2]
--
-- >>> rotate L []
-- []
--
-- >>> rotate R [1]
-- [1]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
rotate :: Direction -> [a] -> [a]
rotate = undefined

-- | `rotations` @grid@ returns a list of grids containing all possible ways 
-- to rotate @grid@. This means the resulting list should normally have 
-- rows + columns many elements. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> rotations (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell False (Add 5), MkCell False (Add 3)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 2), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 3), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 2)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 5)]
--                ]
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
rotations :: Grid -> [Grid]
rotations = undefined

-- | `steps` @grid@ finds the sequence of rotations that lead to a solution 
-- for @grid@ in the fewest number of rotations. The resulting list includes 
-- the solution as the last element. You may assume that this function will
-- never be called on a @grid@ for which there are solutions returned by
-- `solve`. The states of intermediate grids in the resulting list
-- are irrelevant - only the ones of the final grid need to be set correctly.
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 2), MkCell False (Add 3)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 5), MkCell False (Add 2)]
-- >>> steps (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5, 2] [ MkRow 3 [ MkCell False (Add 5), MkCell False (Add 3)] 
--                 , MkRow 4 [ MkCell False (Add 2), MkCell False (Add 2)]
--                 ]  
-- , MkGrid [5, 2] [ MkRow 3 [ MkCell True (Add 3), MkCell False (Add 5)] 
--                 , MkRow 4 [ MkCell True (Add 2), MkCell True (Add 2)]
--                 ] 
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
steps :: Grid -> [Grid]
steps = undefined

--------------------------------------------------------------------------------
