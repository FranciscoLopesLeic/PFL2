module Compiler where

import DataTypes
import Utils
import Data.Char (isDigit)

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA :: Aexp -> Code
compA (Const n) = [Push n]
compA (Var x) = [Fetch x]
compA (AddComp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubComp a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MulComp a1 a2) = compA a1 ++ compA a2 ++ [Mult]

-- compB :: Bexp -> Code
compB :: Bexp -> Code
compB TrueComp = [Tru]
compB FalseComp = [Fals]
compB (Equals a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (LessEq a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (Not bexp) = compB bexp ++ [Neg]
compB (AndComp b1 b2) = compB b1 ++ compB b2 ++ [And]



-- compile :: Program -> Code
compile :: Program -> Code
compile = concatMap compileStm
    where
        compileStm :: Stm -> Code
        compileStm (Assign x a) = compA a ++ [Store x]
        compileStm (If bexp s1 s2) = compB bexp ++ [Branch (compileStm s1) (compileStm s2)]
        compileStm (While bexp s) = [Loop (compB bexp) (compileStm s)]
        compileStm NoopStm = []

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  ALTERAR a lexer que foi copiada                          --
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
lexer :: String -> [String]
lexer [] = []
lexer (':':'=':cs) = ":=" : lexer cs
lexer ('<':'=':cs) = "<=" : lexer cs
lexer ('=':'=':cs) = "==" : lexer cs
lexer ('>':'=':cs) = ">=" : lexer cs
lexer (c:cs)
        | c `elem` " +-*;()=" = if c == ' ' then lexer cs else [c] : lexer cs
        | otherwise = let (word, rest) = span (`notElem` " +-*;()=") (c:cs)
                                    in word : lexer rest


parse :: String -> Program
parse = parseStms.lexer

parseStms :: [String] -> [Stm]
parseStms [] = []
parseStms tokens = 
    let (stm, rest) = parseStm tokens
    in stm : parseStms rest

parseStm :: [String] -> (Stm, [String])
parseStm ("if":rest) = parseIf rest
parseStm ("while":rest) = parseWhile rest
parseStm (var:":=":rest) = parseAssign var rest

parseIf :: [String] -> (Stm, [String])
parseIf tokens = 
    let (cond, rest1) = parseBExp tokens
        (thenStm, "else":rest2) = parseStm rest1
        (elseStm, rest3) = parseStm rest2
    in (If cond thenStm elseStm, rest3)

parseWhile :: [String] -> (Stm, [String])
parseWhile tokens = 
    let (cond, rest1) = parseBExp tokens
        (body, rest2) = parseStm rest1
    in (While cond body, rest2)

parseAssign :: String -> [String] -> (Stm, [String])
parseAssign var tokens = 
    let (expr, ";":rest) = parseAExp tokens
    in (Assign var expr, rest)

parseAExp :: [String] -> (Aexp, [String])
parseAExp (n:rest) | all isDigit n = (Const (read n), rest)
parseAExp (x:"+":rest) = 
    let (a1, rest1) = parseAExp [x]
        (a2, rest2) = parseAExp rest
    in (AddComp a1 a2, rest2)
parseAExp (x:"-":rest) = 
    let (a1, rest1) = parseAExp [x]
        (a2, rest2) = parseAExp rest
    in (SubComp a1 a2, rest2)
parseAExp (x:"*":rest) = 
    let (a1, rest1) = parseAExp [x]
        (a2, rest2) = parseAExp rest
    in (MulComp a1 a2, rest2)
parseAExp (x:rest) = (Var x, rest)


parseBExp :: [String] -> (Bexp, [String])
parseBExp ("True":rest) = (TrueComp, rest)
parseBExp ("False":rest) = (FalseComp, rest)
parseBExp (x:"==":y:rest) = 
    let (a1, _) = parseAExp [x]
        (a2, rest1) = parseAExp [y]
    in (Equals a1 a2, rest)
parseBExp (x:"<=":y:rest) = 
    let (a1, _) = parseAExp [x]
        (a2, rest1) = parseAExp [y]
    in (LessEq a1 a2, rest)
parseBExp ("not":rest) = 
    let (b, rest1) = parseBExp rest
    in (Not b, rest1)
parseBExp (x:"and":y:rest) =
    let (b1, _) = parseBExp [x]
        (b2, rest1) = parseBExp [y]
    in (AndComp b1 b2, rest)
parseBExp tokens = error $ "Invalid boolean expression: " ++ show tokens

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--   where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)