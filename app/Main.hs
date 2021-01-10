module Main where

import Compile
import qualified Data.Text.Lazy.IO as LT
import Parse

main :: IO ()
main = do
  str <- getContents
  let ast = parseStatement str
      llvm = compileToLLVM ast
  LT.putStrLn llvm