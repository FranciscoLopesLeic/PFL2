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

finding :: State -> String -> Maybe StackItem

finding [] _ = Nothing
finding ((str,b):code) c
                          | str == c = Just b
                          | otherwise = finding code c

sorting :: State -> State
sorting = sortBy (comparing fst)

getInteiros :: Maybe StackItem -> StackItem
getInteiros (Just (Inteiro a)) = Inteiro a
getInteiros (Just (Booleano a)) = Booleano a
getInteiros _ = error "Run-time error"

state2Str :: State -> String
state2Str [] = ""
state2Str state = init (recursaostate2Str (sorting state) "")

recursaostate2Str :: State -> String -> String
recursaostate2Str [] acc = acc
recursaostate2Str ((str, Booleano a):code) acc = recursaostate2Str code (acc ++ str ++ "=" ++ show a ++ ",")
recursaostate2Str ((str, Inteiro a):code) acc = recursaostate2Str code (acc ++ str ++ "=" ++ show a ++ ",")

repState :: State -> String -> StackItem -> State
repState state str int = recursaorepState state str int []

recursaorepState :: State -> String -> StackItem -> State -> State
recursaorepState [] str b acc = reverse ((str,b):acc)
recursaorepState ((notstr,b):code) str c acc
                                      | notstr == str = reverse acc ++ ((str, c) : code)
                                      | otherwise = recursaorepState code str c ((notstr,b):acc)