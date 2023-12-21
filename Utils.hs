module Utils where

import DataTypes

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []


stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (x:xs) = stackItem2Str x ++ (if null xs then "" else "," ++ stack2Str xs)

stackItem2Str :: StackItem -> String
stackItem2Str (Inteiro n) = show n
stackItem2Str Verdadeiro = "True"
stackItem2Str Falso = "False"

state2Str :: State -> String
state2Str [] = ""
state2Str ((key, val):xs) = key ++ "=" ++ stackItem2Str val ++ (if null xs then "" else "," ++ state2Str xs)