module Utils where

import DataTypes
import Data.Ord (comparing)
import Data.List ( sortBy, elemIndex )

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []


stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str s = init (recursaostack2Str s "")

recursaostack2Str :: Stack -> String -> String
recursaostack2Str [] acc = acc
recursaostack2Str (Booleano True:rest) acc = recursaostack2Str rest (acc ++ "True,")
recursaostack2Str (Booleano False:rest) acc = recursaostack2Str rest (acc ++ "False,")
recursaostack2Str (Inteiro s:rest) acc = recursaostack2Str rest (acc ++ show s ++ ",")

findState :: State -> String -> Maybe StackItem

findState [] _ = Nothing
findState ((str,b):code) c
                          | str == c = Just b
                          | otherwise = findState code c

sortState :: State -> State
sortState = sortBy (comparing fst)

getJustInt :: Maybe StackItem -> StackItem
getJustInt (Just (Inteiro a)) = Inteiro a
getJustInt (Just (Booleano a)) = Booleano a
getJustInt _ = error "Run-time error"

state2Str :: State -> String
state2Str [] = ""
state2Str state = init (recursaostate2Str (sortState state) "")

recursaostate2Str :: State -> String -> String
recursaostate2Str [] acc = acc
recursaostate2Str ((str, Booleano a):code) acc = recursaostate2Str code (acc ++ str ++ "=" ++ show a ++ ",")
recursaostate2Str ((str, Inteiro a):code) acc = recursaostate2Str code (acc ++ str ++ "=" ++ show a ++ ",")

subsState :: State -> String -> StackItem -> State
subsState state str int = recursaosubsState state str int []

recursaosubsState :: State -> String -> StackItem -> State -> State
recursaosubsState [] str b acc = reverse ((str,b):acc)
recursaosubsState ((notstr,b):code) str c acc
                                      | notstr == str = reverse acc ++ ((str, c) : code)
                                      | otherwise = recursaosubsState code str c ((notstr,b):acc)