{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Compile where

import AST
import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Control.Monad.Fix
import LLVM.AST.IntegerPredicate as P


type Env = M.Map String Operand

compileStatement :: (Control.Monad.Fix.MonadFix m, MonadIRBuilder m) => Statement -> StateT Env m Operand
compileStatement (StateExpr e) = compileExpr e
compileStatement (Assign (Var v) e) = do
    env <- get
    e' <- compileExpr e
    let env' = M.insert v e' env
    put env'
    pure e'

compileExpr :: (Control.Monad.Fix.MonadFix m, MonadIRBuilder m) => Expr -> StateT Env m Operand
compileExpr (ExprInt n) = pure $ int32 n
compileExpr (ExprAdd e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    add e1' e2'
compileExpr (ExprSub e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    sub e1' e2'
compileExpr (ExprMul e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    mul e1' e2'
compileExpr (ExprDiv e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    sdiv e1' e2'
compileExpr (Var v) = do
    env <- get
    let opr = case M.lookup v env of
              Just x  -> x
              Nothing -> error $ "variable " ++ v ++ " not found"
    pure opr
compileExpr (ExprIf condExpr thenExpr elseExpr) = mdo
    cond <- compileExpr condExpr     -- %0 = icmp slt i32 1, 2
    condBr cond ifThen ifElse        -- br i1 %0, label %then, label %else
    -- then:
    ifThen <- block `named` "then"   -- then:
    oprThen <- compileExpr thenExpr  -- then 部分の式を計算する
    br ifEnd                         -- br label %end
    endOfThen <- currentBlock        -- 現在のブロックを enfOfThen とする
    -- else:
    ifElse <- block `named` "else"   -- else:
    oprElse <- compileExpr elseExpr  -- else 部分の式を計算する
    br ifEnd                         -- br label %end
    endOfElse <- currentBlock        -- 現在のブロックを enfOfElse とする
    -- end:
    ifEnd <- block `named` "end"     -- end:
    -- phi
    phi [(oprThen, endOfThen), (oprElse, endOfElse)] -- phi
compileExpr (ExprLT e1 e2) = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  icmp P.SLT e1' e2'
compileExpr (ExprGT e1 e2) = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  icmp P.SGT e1' e2'
compileExpr (ExprEQLT e1 e2) = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  icmp P.SLE e1' e2'
compileExpr (ExprEQGT e1 e2) = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  icmp P.SGE e1' e2'

compileStatements :: (Control.Monad.Fix.MonadFix m, MonadIRBuilder m) => [Statement] -> StateT Env m Operand
compileStatements [e] = compileStatement e
compileStatements (e:es) = do
  compileStatement e
  compileStatements es

compileToLLVM ast =
    ppllvm $ buildModule "main" $ do
    function "main" [] i32 $ \[] -> do
        opr <- evalStateT (compileStatements ast) M.empty
        ret opr