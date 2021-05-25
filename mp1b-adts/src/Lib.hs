---xl69 tianhui2
--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a 
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)
--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer 
eval (IntExp x) = x
eval (PlusExp []) = 0
eval (PlusExp (x:xs)) = eval x + eval (PlusExp xs)
eval (MultExp []) = 1
eval (MultExp (x:xs)) = eval x * eval (MultExp xs)
--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' l = foldr (Cons) Nil l
--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
            | Leaf 
  deriving (Show, Eq)

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node a b c) = a + sumTree b + sumTree c 

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer
             | BoolVal Bool
             | StrVal String
             | ExnVal String
  deriving (Show, Eq)

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal a) (IntVal b) = IntVal (op a b)
liftIntOp _ _ _ = ExnVal "not an IntVal!"
