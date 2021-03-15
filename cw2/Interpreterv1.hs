--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language
import Prelude
import Control.Monad
import Data.Maybe


--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, Int)] --lookup function to load from memory, lecture 18

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError                    -- ^ Division by zero was attempted.
    | NegativeExponentError             -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem

interpret (AssignStmt v e : p) mem = case eval e mem of
        Left x         -> Left x
        Right (Just x) -> interpret p (addOrReplace v x mem)

interpret (IfStmt con body elif e : p) mem = case eval con mem of
        Left x         -> Left x
        Right (Just x) -> if x /= 0 then interpret body mem else (if not (null j) then h else interpret e mem)
        where yo = bruh elif mem
              j = case yo of 
                  Right (Just x) -> x
                  Right Nothing  -> []
              h = case yo of
                  Left x         -> Left x
                  Right (Just x) -> interpret x mem

interpret (RepeatStmt e xs : p) mem = case eval e mem of 
        Left x         -> Left x
        Right (Just x) -> last [interpret xs mem | i <- [0..x]]

                  


--------------------------------------------------------------------------------
bruh :: [(Expr,[Stmt])] -> Memory -> Either Err (Maybe [Stmt])
bruh [] _  = Right Nothing
bruh ((e,s) : p) mem = eval e mem >>= \x ->
    if x /= (Just 0) then Right (Just s) else (if isNothing x then Left DivByZeroError else bruh p mem)


addOrReplace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
addOrReplace key value assoc = (key,value):(filter ((key /=).fst) assoc)


eval :: Expr -> Memory -> Either Err (Maybe Int)
eval (ValE x) _ = Right (Just x)

eval (VarE x) mem = case lookup x mem of
    Nothing -> Left (UninitialisedMemory x)
    Just y  -> Right (Just y)

eval (BinOpE op e1 e2) mem = do
    x <- eval e1 mem
    y <- eval e2 mem
    evalOp op x y
 

evalOp :: Op -> Maybe Int -> Maybe Int -> Either Err (Maybe Int)
evalOp Add (Just x) (Just y) = Right (Just (x + y))

evalOp Sub (Just x) (Just y) = Right(Just (x - y))

evalOp Mul (Just x) (Just y) = Right(Just (x * y))

evalOp Div (Just x) (Just y) = case x `safediv` y of
    Nothing -> Left DivByZeroError
    Just z  -> Right(Just z)

evalOp Pow (Just x) (Just y) = case y >= 0 of
    True  -> Right(Just(x^y))
    False -> Left NegativeExponentError

evalOp Equal (Just x) (Just y) = case x == y of
    True  -> Right(Just 1)
    False -> Right(Just 0)

evalOp Neq (Just x) (Just y) = case x /= y of
    True  -> Right(Just 1)
    False -> Right(Just 0)

evalOp LessThan (Just x) (Just y) = case x < y of
    True  -> Right(Just 1)
    False -> Right(Just 0)

evalOp LessOrEqual (Just x) (Just y) = case x <= y of
    True  -> Right(Just 1)
    False -> Right(Just 0)

evalOp GreaterThan (Just x) (Just y) = case x > y of
    True  -> Right(Just 1)
    False -> Right(Just 0)

evalOp GreaterOrEqual (Just x) (Just y) = case x >= y of
    True  -> Right(Just 1)
    False -> Right(Just 0)


safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)


-- DONE:
-- ASSIGNMENTS     [ok]
-- MEMORY          [ok]
-- ERRORS          [ok]
-- IF/ELSEIF/ELSE  []
-- REPEATS         []
-- FIX CODE        [] lec 21 + lec 22 

-- TO-DO notes:
-- MEMORY: update memory when previously stored identifier is updated [ok]
--         Setting uninitialized variables to other variables sets both to 0
--         by default... why//this shouldn't happen? [??]
-- LOGIC: IF STATEMENTS []
--        REPEATS       []
