module Interpreter where

import DataTypes
import Utils

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push a): codigoRestante, stackRestante, state) = run (codigoRestante, Inteiro a:stackRestante, state)

run (Add: codigoRestante, Inteiro value1:Inteiro value2:stackRestante,state) = run (codigoRestante,Inteiro (value1+value2):stackRestante,state)
run (Sub: codigoRestante, Inteiro value1:Inteiro value2:stackRestante,state) = run (codigoRestante,Inteiro (value1-value2):stackRestante,state)
run (Mult: codigoRestante, Inteiro value1:Inteiro value2:stackRestante,state) = run (codigoRestante,Inteiro (value1*value2):stackRestante,state)
run (Equ: codigoRestante,Inteiro value1:Inteiro value2:stackRestante,state)
                                | value1 == value2 =  run (codigoRestante,Booleano True:stackRestante,state)
                                | otherwise = run (codigoRestante,Booleano False:stackRestante,state)
run (Neg: codigoRestante,Booleano True:stackRestante,state) = run (codigoRestante, Booleano False:stackRestante,state)
run (Neg: codigoRestante,Booleano False:stackRestante,state) = run (codigoRestante, Booleano True:stackRestante,state)
run (And: codigoRestante,Booleano value1:Booleano value2:stackRestante,state)
                                                        | value1 == value2  =  run (codigoRestante,Booleano True:stackRestante,state)
                                                        | otherwise = run (codigoRestante,Booleano False:stackRestante,state)
run (Equ: codigoRestante,Booleano value1:Booleano value2:stackRestante,state)
                                | value1 == value2 =  run (codigoRestante,Booleano True:stackRestante,state)
                                | otherwise = run (codigoRestante,Booleano False:stackRestante,state)
run (Le: codigoRestante,Inteiro value1:Inteiro value2:stackRestante,state)
                                | value1 <= value2 =  run (codigoRestante,Booleano True:stackRestante,state)
                                | otherwise = run (codigoRestante,Booleano False:stackRestante,state)
run (Fals: codigoRestante, stackRestante, state) = run (codigoRestante,Booleano False:stackRestante,state)
run (Tru: codigoRestante, stackRestante, state) = run (codigoRestante,Booleano True:stackRestante,state)
run ((Fetch x): codigoRestante, stackRestante,state) = run (codigoRestante,getInteiros (finding state x):stackRestante,state)
run ((Store x): codigoRestante, value1:stackRestante,state) = run (codigoRestante,stackRestante,repState state x value1)
run (Branch c1 c2: codigoRestante,Booleano boole:stackRestante,state)
                                                    | boole = run (c1 ++ codigoRestante,stackRestante,state)
                                                    | otherwise = run (c2 ++ codigoRestante,stackRestante,state)
run (Noop: codigoRestante,stack,state) = run (codigoRestante,stack,state)
run (Loop c1 c2: codigoRestante,stack,state) = run ((c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]) ++ codigoRestante,stack,state)
run (codigoRestante,stack,state) = error $ "Run-time error"