module HW05 where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s :: Maybe ExprT of
              Just x -> Just $ eval x
              Nothing -> Nothing
