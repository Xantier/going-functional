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
