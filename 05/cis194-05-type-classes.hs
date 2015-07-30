module Calc where

import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Lit a) = a

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = monadize . parseExp Lit Add Mul
  where monadize Nothing  = Nothing
        monadize (Just e) = Just $ eval e
	  
-- exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
  
instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul
  
-- exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)
  
instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)
  
newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y)= MinMax (max x y)
  mul (MinMax x) (MinMax y)= MinMax (min x y)
  
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
