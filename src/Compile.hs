{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Compile where

import AST
import Control.Monad.Fix
import Control.Monad.Trans.State
import qualified Data.Map as M
import Data.String
import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.AddrSpace
import LLVM.AST.Constant as AST
import LLVM.AST.IntegerPredicate as P
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

type Env = M.Map String Operand

compileStatement :: (MonadModuleBuilder m, Control.Monad.Fix.MonadFix m, MonadIRBuilder m) => Statement -> StateT Env m Operand
compileStatement (StateExpr e) = compileExpr e
compileStatement (Assign (Var v) e) = do
    env <- get
    e' <- compileExpr e
    let env' = M.insert v e' env
    put env'
    pure e'
compileStatement (FunDef funName paramNames funBody) = do
  env <- get
      -- 返り値の型が i32 で、引数の型が (i32, ...) であるような関数の型
  let typ = FunctionType i32 (replicate (length paramNames) i32) False
      -- そんな関数へのポインターの型
      ptrTyp = AST.PointerType typ (AddrSpace 0)
      -- 関数名と、関数へのポインターを組にした参照
      ref = AST.GlobalReference ptrTyp (fromString funName)
      -- 関数名と関数への参照の対応を環境に追加する
      env' = M.insert funName (ConstantOperand ref) env
  put env'
  function (fromString funName) (zip (repeat i32) (map fromString paramNames)) i32 $ \oprs -> do
      -- "仮引数と実引数の対応"を、環境に追加する
      let newEnv = foldr (\(k,v) env -> M.insert k v env) env' (zip paramNames oprs)
      -- 新しい環境で本体をコンパイルする
      opr <- evalStateT (compileExpr funBody) newEnv
      ret opr


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
compileExpr (ExprEQ e1 e2) = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  icmp P.EQ e1' e2'
compileExpr (FunCall funName exprs) = do
  env <- get                           -- 環境を取り出して
  oprs <- mapM compileExpr exprs       -- 引数を順にコンパイルする
  let f = case M.lookup funName env of -- 環境から関数を探して
          Just funOperand -> funOperand    -- あればそれを使う。なければエラー
          Nothing -> error $ "function " ++ funName ++ " not found"
  call f (zip oprs (repeat []))        -- 引数の計算結果をもとに関数を呼ぶ

compileStatements :: (MonadModuleBuilder m, Control.Monad.Fix.MonadFix m, MonadIRBuilder m) => [Statement] -> StateT Env m Operand
compileStatements [e] = compileStatement e
compileStatements (e:es) = do
  compileStatement e
  compileStatements es

compileToLLVM ast =
    ppllvm $ buildModule "main" $ do
    function "main" [] i32 $ \[] -> do
        opr <- evalStateT (compileStatements ast) M.empty
        ret opr