module AST where

data Expr =            -- 式
   ExprAdd Expr Expr   -- 式 + 式
   | ExprSub Expr Expr -- 式 - 式
   | ExprInt Integer   -- 式の中身が整数一個
   deriving Show