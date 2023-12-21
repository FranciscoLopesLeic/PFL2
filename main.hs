module Main where

import Interpreter
import DataTypes
import Utils
import Tests( runTests )

main :: IO ()
main = do
  runTests
