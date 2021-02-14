--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module Game where 

import Prelude
import Data.List
import Control.Monad

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
-- cons the elements of the recursive call to candidates.
---------------------------------------

candidates :: [Cell] -> [[Cell]]
candidates [] = [[]]
candidates (x:xs) = [z:y | z <- states x, y <- ys]
    where ys = candidates xs

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
---------------------------------------
-- BITCH IM A COW BITCH IM A COW BITCH IM A COW BITCH IM A COW BITCH IM A COW
---------------------------------------

solve :: Grid -> [Grid]
solve grid@(MkGrid targets rows) = final
    where solutions = solveforallrows rows 
          results   = [MkGrid targets solution | solution <- solutions]
          columns   = [transpose (getcolumns solution) | solution <- solutions] 
          ints      = [solveforallcolumns targets column | column <- columns]  
          indices   = elemIndices True ints
          final     = if indices == [] then [grid] else [results !! index | index <- indices]

-- Given the targets for the columns and the result of each column's sum, compares
-- whether the two arrays are the same and returns a boolean value.
solveforallcolumns :: [Int] -> [[Cell]] -> Bool
solveforallcolumns targets columns =
     targets == results
    where results = map result columns

solveforallrows :: [Row] -> [[Row]]
solveforallrows xs = sequence ys
    where ys       = map solveRow xs

getcolumns :: [Row] -> [[Cell]]
getcolumns =  map getcells

getcells :: Row -> [Cell]
getcells (MkRow _ cells) = cells


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
rotate _ [] = []
rotate _ [x] = [x]
rotate L xs = b ++ a
    where (a, b) = splitAt 1 xs
rotate R xs = ([last xs] ++ take (length xs - 1) xs)

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
-- rotationz (MkGrid [5, 2] [MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)], MkRow 4 [MkCell False (Add 2), MkCell False (Add 7)]])
-- rotationz (MkGrid [5, 2, 4] [MkRow 3 [MkCell False (Add 3), MkCell False (Add 5), MkCell False (Add 7)], MkRow 4 [MkCell False (Add 2), MkCell False (Add 2), MkCell False (Add 10)], MkRow 90 [MkCell False (Add 8), MkCell False (Add 5), MkCell False (Add 9)]])
-- rotations (MkGrid [ 0 , 0 ] [ MkRow 0 [ MkCell False (Add 0) , MkCell False (Add 0) ] , MkRow 0 [ MkCell False (Add 0) , MkCell False (Add 0) ]])
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------

rotations :: Grid -> [Grid]
rotations grid@(MkGrid [] _) = [grid]
rotations (MkGrid ns rows) = [MkGrid ns bruh | bruh <- final]
    where rowz     = functionList 0 rowcells
          rowcells = [getcells row | row <- rows]
          columncells = transpose rowcells
          columnz = [transpose y| y <- functionList 0 columncells]
          result = rowz ++ columnz
          final = [helper rows grid | grid <- result]


functionList :: Int -> [[a]] -> [[[a]]]
functionList k [] = [[]]
functionList k xs = if k < length xs then [functionListHelp (rotate L) lhs rhs]++(functionList (k+1) xs) else [] 
    where lhs = take k xs 
          rhs = drop k xs

-- THIS IS BASICALLY MAP WITH STEPS
functionListHelp :: (a -> a) -> [a] -> [a] -> [a]
functionListHelp f [] [] = []
functionListHelp f [] (y:ys) = (f y): ys 
functionListHelp f xs (y:ys) = xs ++ (f y) : ys 


helper :: [Row] -> [[Cell]] -> [Row]
helper [] [] = []
helper (x:xs) (y:ys) = setcell x y : helper xs ys
    where setcell (MkRow n ys) zs = MkRow n zs


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
