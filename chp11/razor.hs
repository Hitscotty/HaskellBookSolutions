module Razor where

data Expr
  = Lit Integer
  | Add Expr Expr
  deriving Show


a1 = Add  (Lit 9001) (Lit 1)
a2 = Add  a1 (Lit 20001)
a3 = Add  (Lit 1) a2
  
eval :: Expr -> Integer
eval (Lit a)   = a
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit a) =  show a
printExpr (Add x y) = printExpr x ++ " + " ++  printExpr y
