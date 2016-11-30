module HuttonsRazor where

-- 1. Your first task is to write the “eval” function which reduces an expression to a final sum.
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n)   = n
eval (Add x y) = eval x + eval y

-- 2. Write a printer for the expressions.
printExpr :: Expr -> String
printExpr (Lit n)   = show n
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
