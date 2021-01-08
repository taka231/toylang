{-# LANGUAGE OverloadedStrings #-}
module Compile where

import AST
import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

compile :: (MonadIRBuilder m) => Expr -> m Operand
compile (ExprInt n) = pure $ int32 n
compile (ExprAdd e1 e2) = do
    e1' <- compile e1
    e2' <- compile e2
    add e1' e2'
compile (ExprSub e1 e2) = do
    e1' <- compile e1
    e2' <- compile e2
    sub e1' e2'

compileToLLVM ast =
    ppllvm $ buildModule "main" $ do
    function "main" [] i32 $ \[] -> do
        opr <- compile ast
        ret opr