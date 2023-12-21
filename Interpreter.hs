module Interpreter where

import DataTypes
import Utils

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (Push n:codigoRestante, stack, state) = run (codigoRestante, Inteiro n:stack, state)

run (Add:codigoRestante, Inteiro i1:Inteiro i2:stack, state) = run (codigoRestante, Inteiro (i1 + i2):stack, state)
run (Add:codigoRestante, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Mult:codigoRestante, Inteiro i1:Inteiro i2:stack, state) = run (codigoRestante, Inteiro (i1 * i2):stack, state)
run (Mult:codigoRestante, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Sub:codigoRestante, Inteiro i1:Inteiro i2:stack, state) = run (codigoRestante, Inteiro (i1 - i2):stack, state)
run (Sub:codigoRestante, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Tru:codigoRestante, stack, state) = run (codigoRestante, Verdadeiro:stack, state)

run (Fals:codigoRestante, stack, state) = run (codigoRestante, Falso:stack, state)

run (Equ:codigoRestante, value1:value2:stack, state) = run (codigoRestante, value:stack, state)
    where value = if value1 == value2 then Verdadeiro else Falso

run (Le:codigoRestante, Inteiro i1:Inteiro i2:stack, state) = run (codigoRestante, value:stack, state)
    where value = if i1 <= i2 then Verdadeiro else Falso
run (Le:codigoRestante, _:_:stack, state) = error "Runtime error: Invalid operation"

run (And:codigoRestante, Verdadeiro:Verdadeiro:stack, state) = run (codigoRestante, Verdadeiro:stack, state)
run (And:codigoRestante, Verdadeiro:Falso:stack, state) = run (codigoRestante, Falso:stack, state)
run (And:codigoRestante, Falso:Verdadeiro:stack, state) = run (codigoRestante, Falso:stack, state)
run (And:codigoRestante, Falso:Falso:stack, state) = run (codigoRestante, Falso:stack, state)
run (And:codigoRestante, Inteiro i:stack, state) = error "Runtime error: Invalid operation"

run (Neg:codigoRestante, Verdadeiro:stack, state) = run (codigoRestante, Falso:stack, state)
run (Neg:codigoRestante, Falso:stack, state) = run (codigoRestante, Verdadeiro:stack, state)
run (Neg:codigoRestante, Inteiro i:stack, state) = error "Runtime error: Invalid operation"

run (Fetch key:codigoRestante, stack, state) =
    case lookup key state of
        Just value -> run (codigoRestante, value:stack, state)
        Nothing -> error "Chave não encontrada no estado"

run (Store key:codigoRestante, value:stack, state) = 
    run (codigoRestante, stack, atualizarEstado key value state)

run (Branch code1 code2:codigoRestante, Verdadeiro:stackRestante, state) = run (code1, stackRestante, state)
run (Branch code1 code2:codigoRestante, Falso:stackRestante, state) = run (code2, stackRestante, state)
run (Branch code1 code2:codigoRestante, Inteiro i:stackRestante, state) = error "Runtime error: Invalid operation"

run (Loop code1 code2:codigoRestante, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]], stack, state)

run (Noop:codigoRestante, stack, state) = run (codigoRestante, stack, state)

atualizarEstado :: Key -> Value -> State -> State
atualizarEstado chave valor estado = (chave, valor) : filter ((/= chave) . fst) estado