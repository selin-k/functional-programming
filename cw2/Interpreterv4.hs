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
    | NegativeExprRepeatError           -- ^ If the loop guard of a RepeatStmt
                                        -- expression is a negative number.
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem

interpret (AssignStmt v e : p) mem = do
    x <- eval e mem
    interpret p (addOrReplace v x mem)

interpret (IfStmt con body elif els : p) mem = do
    x <- eval con mem
    if x /= 0 then interpret body mem 
    else elseHelper elif mem els

interpret (RepeatStmt e xs : p) mem = do
    x <- eval e mem 
    if x >= 0 then iterator x xs mem 
    else Left NegativeExprRepeatError


--------------------------------------------------------------------------------
-- recursives with monads??
iterator :: Int -> [Stmt] -> Memory -> Either Err Memory
iterator 0 _ mem  = Right mem
iterator n xs mem = do
    x <- interpret xs mem
    iterator (n-1) xs x

-- how to reduce on the if else statements??
elseHelper :: [(Expr,[Stmt])] -> Memory -> [Stmt] -> Either Err Memory
elseHelper [] mem els          = interpret els mem
elseHelper ((e,s) : p) mem els = do 
    x <- eval e mem 
    if x /= 0 then interpret s mem 
    else elseHelper p mem els


-- simplify // more efficient ??
addOrReplace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
addOrReplace key value assoc = (key,value):(filter ((key /=) . fst) assoc)


-- can i rid of the case of here??
eval :: Expr -> Memory -> Either Err Int
eval (ValE x) _ = Right x

eval (VarE x) mem = case lookup x mem of
    Nothing -> Left (UninitialisedMemory x)
    Just y  -> Right y

eval (BinOpE op e1 e2) mem = do
    x <- eval e1 mem
    y <- eval e2 mem
    evalOp op x y
 

-- can i cut down on the length of this at all?? 
-- any other way apart from pattern matching??
evalOp :: Op ->  Int ->  Int -> Either Err Int
evalOp Add x y = Right (x + y)

evalOp Sub x y = Right(x - y)

evalOp Mul x y = Right(x * y)

evalOp Div x y = case x `safediv` y of
    Nothing -> Left DivByZeroError
    Just z  -> Right z

evalOp Pow x y = case y >= 0 of
    True  -> Right(x^y)
    False -> Left NegativeExponentError

evalOp Equal x y = case x == y of
    True  -> Right 1
    False -> Right 0

evalOp Neq x y = case x /= y of
    True  -> Right 1
    False -> Right 0

evalOp LessThan x y = case x < y of
    True  -> Right 1
    False -> Right 0

evalOp LessOrEqual x y = case x <= y of
    True  -> Right 1
    False -> Right 0

evalOp GreaterThan x y = case x > y of
    True  -> Right 1
    False -> Right 0

evalOp GreaterOrEqual x y = case x >= y of
    True  -> Right 1
    False -> Right 0


safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)


-- DONE:
-- ASSIGNMENTS     [ok]
-- MEMORY          [ok]
-- ERRORS          [ok]
-- IF/ELSEIF/ELSE  [ok]
-- REPEATS         [ok]
-- FIX CODE        [1/2] lec 21 + lec 22 ++ look at Op type classes to simplify...
-- EXTENSIONS      []

-- TO-DO notes:
-- MEMORY: update memory when previously stored identifier is updated [ok]
--         Setting uninitialized variables to other variables sets both to 0
--         by default... why//this shouldn't happen? [??]
-- LOGIC: IF STATEMENTS [ok]
--        REPEATS       [ok]
