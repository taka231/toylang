{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Internal.Lazy
import Data.Functor.Identity
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

main :: IO ()
main = do
    n <- read <$> getLine   -- 標準入力から1行を読んで整数として解釈する
    LT.putStrLn $ ppllvm $  -- LLVM IRをテキスト表現として表示する
        buildModule "main" $ do
            function "main" [] i32 $ \[] ->
                ret (int32 n)