module Main where

import Interpreter
import DataTypes
import Utils
import Compiler
import Tests( runTests )

main :: IO ()
main = do
  runTests
