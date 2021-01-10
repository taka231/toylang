module AST where

data Expr =            -- 式
   ExprAdd Expr Expr   -- 式 + 式
   | ExprSub Expr Expr -- 式 - 式
   | ExprMul Expr Expr -- 式 * 式
   | ExprDiv Expr Expr -- 式 / 式
   | ExprInt Integer   -- 式の中身が整数一個
   | Var String
   | ExprIf Expr Expr Expr
   | ExprLT Expr Expr
   | ExprGT Expr Expr
   | ExprEQLT Expr Expr
   | ExprEQGT Expr Expr
   deriving Show

data Statement =
   Assign Expr Expr
   | StateExpr Expr
   deriving Show