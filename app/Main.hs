module Main where

import           Compile
import           Control.Spoon
import qualified Data.Text.Lazy.IO  as LT
import           Parse
import           System.Environment (getArgs)

main :: IO ()
main = do
  str <- getContents
  args <- getArgs
  let ast = parseStatement str
      llvm = compileToLLVM ast
  if spoon (head args) == Just "--printAST" then print ast else pure ()
  LT.putStrLn llvm
