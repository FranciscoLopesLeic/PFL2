module Compiler where

import DataTypes
import Debug.Trace
import Data.List ( sortBy, elemIndex, isPrefixOf )
import Data.Ord (comparing)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Utils
import Data.Char (isDigit, isAlpha, isSpace, isAlphaNum)

-- Part 2
-- TODO: Define the types Aexp, Bexp, Stm and Program
-- compA :: Aexp -> Code

compA :: Aexp -> Code
compA (Num a) = [Push a]
compA (Var a) = [Fetch a]
compA (AddComp a b) = compA b ++ compA a ++ [Add]
compA (SubComp a b) = compA b ++ compA a ++ [Sub]
compA (MulComp a b) = compA b ++ compA a ++ [Mult]

compB :: Bexp -> Code
compB (Equals a b) = compA a ++ compA b ++ [Equ]
compB (LessEq a b) = compA b ++ compA a ++ [Le]
compB (AndComp a b) = compB a ++ compB b ++ [And]
compB (NegComp a) = compB a ++ [Neg]
compB (EqualsBool a b) = compB a ++ compB b ++ [Equ]
compB TrueComp = [Tru]
compB FalseComp = [Fals]

compile :: [Stm] -> Code
compile program = 
    let compiledCode = compileStm program
    in trace ("Compiled code: " ++ show compiledCode) compiledCode

compileStm :: [Stm] -> Code
compileStm [] = []
compileStm (stm:rest) = case stm of
  Assign var aexp -> compA aexp ++ [Store var] ++ compileStm rest
  BranchS bexp stm1 stm2  -> compB bexp ++ [Branch (compileStm stm1) (compileStm stm2)] ++ compileStm rest
  LoopS bexp stm     -> Loop (compB bexp) (compileStm stm) : compileStm rest
  If bexp stm1 stm2 -> compB bexp ++ [Branch (compileStm [stm1]) (compileStm [stm2])] ++ compileStm rest
  Seq stm -> compileStm stm ++ compileStm rest

parse :: String -> Program
parse str = auxiliar (debugger str) []

auxiliar :: [String] -> [Stm] -> [Stm]
auxiliar [] stm = stm
auxiliar (a:":=":rest) stm = let x = findjust (elemIndex ";" (a:":=":rest))
                              in case parseAddOrMulOrInteiroOrPar (drop 2 (take (x-1) (a:":=":rest))) of -- Case Add/Mul/inteiro/Par
                                Just (expr,[]) -> auxiliar (drop x (a:":=":rest)) (stm++[Assign a expr]) -- Case Add/Mul/inteiro/Par
                                Nothing -> error "Parse Error" -- Case Add/Mul/inteiro/Par
                                _ -> error "Parse Error" -- Case Add/Mul/inteiro/Par
auxiliar ("(":rest) stm = auxiliar (drop (findjust (elemIndex ")" ("(":rest))) ("(":rest)) (stm++auxiliar (drop 1 (take (findjust (elemIndex ")" ("(":rest))-1) ("(":rest))) [])
auxiliar (";":rest) stm = auxiliar rest stm
auxiliar ("if":rest) stm = let indexOfThen = findjust (elemIndex "then" ("if":rest))
                               indexOfElse = findjust (elemIndex "else" ("if":rest))
                               postElseArray = drop indexOfElse ("if":rest)
                            in case limparPrimeiro postElseArray of
                              "(" -> auxiliar (drop (findjust (elemIndex ")" postElseArray)) postElseArray) (stm++[BranchS (findjustB (parseEqualsBoolandAndComp (isPar (drop 1 (take (indexOfThen-1) ("if":rest)))))) (auxiliar (drop indexOfThen (take (indexOfElse-1) ("if":rest))) []) (auxiliar (take (findjust (elemIndex ")" postElseArray)) postElseArray ) [] )]) -- Case EqualsBool and AndComp
                              _  -> auxiliar (drop (findjust (elemIndex ";" postElseArray)) postElseArray) (stm++[BranchS (findjustB (parseEqualsBoolandAndComp (isPar (drop 1 (take (indexOfThen-1) ("if":rest)))))) (auxiliar (drop indexOfThen (take (indexOfElse-1) ("if":rest))) []) (auxiliar (take (findjust (elemIndex ";" postElseArray)) postElseArray ) [] )]) -- Case EqualsBool and AndComp
auxiliar ("while":rest) stm = let dopos = findjust (elemIndex "do" ("while":rest))
                                  postElseArray = drop dopos ("while":rest)
                              in case limparPrimeiro postElseArray of
                                "(" -> auxiliar (drop (findjust (elemIndex ")" postElseArray)) postElseArray) (stm++[LoopS (findjustB (parseEqualsBoolandAndComp (isPar (drop 1 (take (dopos-1) ("while":rest)))))) (auxiliar (take (findjust (elemIndex ")" postElseArray)) postElseArray ) [] )]) -- Case EqualsBool and AndComp
                                _ -> auxiliar (drop (findjust (elemIndex ";" postElseArray)) postElseArray) (stm++[LoopS (findjustB (parseEqualsBoolandAndComp (isPar (drop 1 (take (dopos-1) ("while":rest)))))) (auxiliar (take (findjust (elemIndex ";" postElseArray)) postElseArray ) [] )]) -- Case EqualsBool and AndComp


findjustB :: Maybe (Bexp,[String]) -> Bexp
findjustB (Just (a,[")"])) = a
findjustB (Just (a,[])) = a
findjustB Nothing = error "Parse Error"

isPar :: [String] -> [String]
isPar ("(":rest) = drop 1 (take (length ("(":rest)) ("(":rest))
isPar rest = rest

limparPrimeiro :: [String] -> String
limparPrimeiro ("(":rest) = "("
limparPrimeiro (a:rest) = a

parseInteiro :: [String] -> Maybe (Aexp,[String])
parseInteiro (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseInteiro _ = Nothing

parseInteiroOrMul :: [String] -> Maybe(Aexp,[String])
parseInteiroOrMul str =
  case parseInteiro str of
    Just (firstExp,"*":rest1) ->
      case parseInteiroOrMul rest1 of
        Just (secondExp,rest2) ->
          Just (MulComp firstExp secondExp,rest2)
        Nothing                  -> Nothing
    result -> result

parseInteiroOrAddOrMult :: [String] -> Maybe(Aexp,[String])
parseInteiroOrAddOrMult str =
  case parseInteiroOrMul str of
    Just (firstExp,"+":rest1) ->
      case parseInteiroOrAddOrMult rest1 of
        Just (secondExp,rest2) ->
          Just (AddComp firstExp secondExp,rest2)
        Nothing                  -> Nothing
    Just (firstExp,"-":rest1) ->
      case parseInteiroOrAddOrMult rest1 of
        Just (secondExp,rest2) ->
          Just (SubComp firstExp secondExp,rest2)
        Nothing                  -> Nothing
    result -> result

parseParentOrInteiro :: [String] -> Maybe (Aexp,[String])
parseParentOrInteiro ("(":rest) =
  case parseAddOrMulOrInteiroOrPar rest of
    Just (expr,")":rest1) -> Just (expr,rest1)
    Just _ -> Nothing
    Nothing -> Nothing
parseParentOrInteiro (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseParentOrInteiro _ = Nothing

parseMulOrInteiroOrPar :: [String] -> Maybe (Aexp,[String])
parseMulOrInteiroOrPar rest =
  case parseParentOrInteiro rest of
    Just (firstExp,"*":rest1) ->
      case parseMulOrInteiroOrPar rest1 of
        Just (secondExp,rest2) -> Just (MulComp firstExp secondExp, rest2)
        Nothing -> Nothing
    result -> result

parseAddOrMulOrInteiroOrPar :: [String] -> Maybe (Aexp,[String])
parseAddOrMulOrInteiroOrPar rest =
  case parseMulOrInteiroOrPar rest of
    Just (firstExp,"+":rest1) ->
      case parseAddOrMulOrInteiroOrPar rest1 of
        Just (secondExp,rest2) -> Just (AddComp firstExp secondExp, rest2)
        Nothing -> Nothing
    Just (firstExp,"-":rest1) ->
      case parseAddOrMulOrInteiroOrPar rest1 of
        Just (secondExp,rest2) -> Just (SubComp firstExp secondExp, rest2)
        Nothing -> Nothing
    result -> result

------------- PARSE Bexp ----------------

parseAnyOperatorB :: [String] -> Maybe (Bexp,[String])
parseAnyOperatorB ("(":rest) =
  case parseEqualsBoolandAndComp rest of
    Just (expr,")":rest1) -> Just (expr,rest1)
    Just _ -> Nothing
    Nothing -> Nothing
parseAnyOperatorB ("True":rest) = Just (TrueComp,rest)
parseAnyOperatorB ("False":rest) = Just (FalseComp,rest)
parseAnyOperatorB rest =
  case parseAddOrMulOrInteiroOrPar rest of
    Just (firstExp,"<=":rest1) ->
      case parseAddOrMulOrInteiroOrPar rest1 of
        Just (secondExp,rest2) ->
          Just (LessEq firstExp secondExp, rest2)
        Nothing -> Nothing
    Just (firstExp,"==":rest1) ->
      case parseAddOrMulOrInteiroOrPar rest1 of
        Just (secondExp,rest2) ->
          Just (Equals firstExp secondExp, rest2)
        Nothing -> Nothing
    result -> Nothing

parseLessEqAndEqualsAndNegComp :: [String] -> Maybe(Bexp, [String])
parseLessEqAndEqualsAndNegComp ("not":rest) =
    case parseAnyOperatorB rest of
      Just (firstExp,rest1) ->
        Just (NegComp firstExp,rest1)
      result -> result
parseLessEqAndEqualsAndNegComp rest = parseAnyOperatorB rest

parseEqualsBoolandNegComp :: [String] -> Maybe(Bexp, [String])
parseEqualsBoolandNegComp rest =
  case parseLessEqAndEqualsAndNegComp rest of
    Just (firstExp, "=":rest1) ->
      case parseEqualsBoolandNegComp rest1 of
        Just (secondExp, rest2) ->
          Just (EqualsBool firstExp secondExp, rest2)
        Nothing -> Nothing
    result -> result

parseEqualsBoolandAndComp :: [String] -> Maybe(Bexp,[String])
parseEqualsBoolandAndComp rest =
  case parseEqualsBoolandNegComp rest of
    Just (firstExp, "and":rest1) ->
      case parseEqualsBoolandAndComp rest1 of
        Just (secondExp, rest2) ->
          Just (AndComp firstExp secondExp, rest2)
        Nothing -> Nothing
    result -> result

findjust :: Num a => Maybe a -> a
findjust (Just a) = a+1

keywords :: [String]
keywords = ["then", "while", "if", "do", "not", "else", "and"]

operadores :: [String]
operadores = ["==", ":=", "<=", "+", "-", "*", "/", "="]

lexer :: String -> [String]
lexer [] = []
lexer str@(c:cs)
    | isSpace c = lexer cs
    | isAlpha c = let (palavra, rest) = span isAlphaNum str in palavra : lexer rest
    | isDigit c = let (digito, rest) = span isDigit str in digito : lexer rest
    | otherwise = lexerSymbol str

lexerSymbol :: String -> [String]
lexerSymbol [] = []
lexerSymbol str = case getmatch str operadores of
    Just op -> op : lexer (drop (length op) str)
    Nothing -> [head str] : lexer (tail str)

getmatch :: String -> [String] -> Maybe String
getmatch str = foldr (\op acc -> if op `isPrefixOf` str then Just op else acc) Nothing

convertToString :: Simbolo -> String
convertToString (Keyword kw) = kw
convertToString (Operador op) = op
convertToString (Symbol simbolo) = simbolo
convertToString (Identifier id) = id
convertToString (Number num) = num

debugger :: String -> [String]
debugger input = let simbolos = lexer input
                   in traceShow simbolos simbolos