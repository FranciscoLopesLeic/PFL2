module Compiler where

import DataTypes
import Debug.Trace
import Data.List ( sortBy, elemIndex )
import Data.Ord (comparing)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Utils
import Data.Char (isDigit, isAlpha)

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

compile :: Program -> Code
compile = concatMap compileStm

compileStm :: Stm -> Code
compileStm stm = case stm of
  Assign var aexp -> compA aexp ++ [Store var]
  BranchS bexp stm1 stm2  -> compB bexp ++ [Branch (compile stm1) (compile stm2)]
  LoopS bexp stm     -> [Loop (compB bexp) (compile stm)]

parse :: String -> Program
parse str = parseaux (lexer str) []

parseaux :: [String] -> [Stm] -> [Stm]
parseaux [] stm = stm
parseaux (a:":=":rest) stm = let x = findjust (elemIndex ";" (a:":=":rest))
                              in case parseSumOrProdOrIntOrPar (drop 2 (take (x-1) (a:":=":rest))) of
                                Just (expr,[]) -> parseaux (drop x (a:":=":rest)) (stm++[Assign a expr])
                                Nothing -> error "Parse Error"
                                _ -> error "Parse Error"
parseaux ("(":rest) stm = parseaux (drop (findjust (elemIndex ")" ("(":rest))) ("(":rest)) (stm++parseaux (drop 1 (take (findjust (elemIndex ")" ("(":rest))-1) ("(":rest))) [])
parseaux (";":rest) stm = parseaux rest stm
parseaux ("if":rest) stm = let thenpos = findjust (elemIndex "then" ("if":rest))
                               elsepos = findjust (elemIndex "else" ("if":rest))
                               arrayafter = drop elsepos ("if":rest)
                            in case takefirstelement arrayafter of
                              "(" -> parseaux (drop (findjust (elemIndex ")" arrayafter)) arrayafter) (stm++[BranchS (findjustB (parseEqualsBoolandAndComp (checkifPar (drop 1 (take (thenpos-1) ("if":rest)))))) (parseaux (drop thenpos (take (elsepos-1) ("if":rest))) []) (parseaux (take (findjust (elemIndex ")" arrayafter)) arrayafter ) [] )])
                              _  -> parseaux (drop (findjust (elemIndex ";" arrayafter)) arrayafter) (stm++[BranchS (findjustB (parseEqualsBoolandAndComp (checkifPar (drop 1 (take (thenpos-1) ("if":rest)))))) (parseaux (drop thenpos (take (elsepos-1) ("if":rest))) []) (parseaux (take (findjust (elemIndex ";" arrayafter)) arrayafter ) [] )])
parseaux ("while":rest) stm = let dopos = findjust (elemIndex "do" ("while":rest))
                                  arrayafter = drop dopos ("while":rest)
                              in case takefirstelement arrayafter of
                                "(" -> parseaux (drop (findjust (elemIndex ")" arrayafter)) arrayafter) (stm++[LoopS (findjustB (parseEqualsBoolandAndComp (checkifPar (drop 1 (take (dopos-1) ("while":rest)))))) (parseaux (take (findjust (elemIndex ")" arrayafter)) arrayafter ) [] )])
                                _ -> parseaux (drop (findjust (elemIndex ";" arrayafter)) arrayafter) (stm++[LoopS (findjustB (parseEqualsBoolandAndComp (checkifPar (drop 1 (take (dopos-1) ("while":rest)))))) (parseaux (take (findjust (elemIndex ";" arrayafter)) arrayafter ) [] )])


findjustB :: Maybe (Bexp,[String]) -> Bexp
findjustB (Just (a,[")"])) = a
findjustB (Just (a,[])) = a
findjustB Nothing = error "Parse Error"

checkifPar :: [String] -> [String]
checkifPar ("(":rest) = drop 1 (take (length ("(":rest)) ("(":rest))
checkifPar rest = rest

takefirstelement :: [String] -> String
takefirstelement ("(":rest) = "("
takefirstelement (a:rest) = a

parseInt :: [String] -> Maybe (Aexp,[String])
parseInt (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseInt _ = Nothing

parseProdOrInt :: [String] -> Maybe(Aexp,[String])
parseProdOrInt str =
  case parseInt str of
    Just (firstExp,"*":rest1) ->
      case parseProdOrInt rest1 of
        Just (secondExp,rest2) ->
          Just (MulComp firstExp secondExp,rest2)
        Nothing                  -> Nothing
    result -> result

parseSumOrProdOrInt :: [String] -> Maybe(Aexp,[String])
parseSumOrProdOrInt str =
  case parseProdOrInt str of
    Just (firstExp,"+":rest1) ->
      case parseSumOrProdOrInt rest1 of
        Just (secondExp,rest2) ->
          Just (AddComp firstExp secondExp,rest2)
        Nothing                  -> Nothing
    Just (firstExp,"-":rest1) ->
      case parseSumOrProdOrInt rest1 of
        Just (secondExp,rest2) ->
          Just (SubComp firstExp secondExp,rest2)
        Nothing                  -> Nothing
    result -> result

parseIntOrParentExpr :: [String] -> Maybe (Aexp,[String])
parseIntOrParentExpr ("(":rest) =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr,")":rest1) -> Just (expr,rest1)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntOrParentExpr (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseIntOrParentExpr _ = Nothing

parseProdOrIntOrPar :: [String] -> Maybe (Aexp,[String])
parseProdOrIntOrPar rest =
  case parseIntOrParentExpr rest of
    Just (firstExp,"*":rest1) ->
      case parseProdOrIntOrPar rest1 of
        Just (secondExp,rest2) -> Just (MulComp firstExp secondExp, rest2)
        Nothing -> Nothing
    result -> result

parseSumOrProdOrIntOrPar :: [String] -> Maybe (Aexp,[String])
parseSumOrProdOrIntOrPar rest =
  case parseProdOrIntOrPar rest of
    Just (firstExp,"+":rest1) ->
      case parseSumOrProdOrIntOrPar rest1 of
        Just (secondExp,rest2) -> Just (AddComp firstExp secondExp, rest2)
        Nothing -> Nothing
    Just (firstExp,"-":rest1) ->
      case parseSumOrProdOrIntOrPar rest1 of
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
  case parseSumOrProdOrIntOrPar rest of
    Just (firstExp,"<=":rest1) ->
      case parseSumOrProdOrIntOrPar rest1 of
        Just (secondExp,rest2) ->
          Just (LessEq firstExp secondExp, rest2)
        Nothing -> Nothing
    Just (firstExp,"==":rest1) ->
      case parseSumOrProdOrIntOrPar rest1 of
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

lexer :: String -> [String]
lexer string = lexeraux string [] []

lexeraux :: String -> [String] -> String -> [String]
lexeraux [] aux auxiliar | auxiliar == "" =  aux
                       | otherwise = aux++[auxiliar]
lexeraux ('w':'h':'i':'l':'e':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["while"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["while"]) []
lexeraux (' ':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest aux []
                            | otherwise = lexeraux rest (aux++[auxiliar]) []
lexeraux ('i':'f':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["if"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["if"]) []
lexeraux ('t':'h':'e':'n':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["then"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["then"]) []
lexeraux ('e':'l':'s':'e':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["else"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["else"]) []
lexeraux ('*':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["*"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["*"]) []
lexeraux ('+':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["+"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["+"]) []
lexeraux ('/':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["/"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["/"]) []
lexeraux ('-':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["-"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["-"]) []
lexeraux (';':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++[";"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++[";"]) []
lexeraux ('(':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["("]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["("]) []
lexeraux (')':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++[")"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++[")"]) []
lexeraux ('<':'=':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["<="]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["<="]) []
lexeraux ('=':'=':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["=="]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["=="]) []
lexeraux ('n':'o':'t':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["not"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["not"]) []
lexeraux ('=':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["="]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["="]) []
lexeraux ('a':'n':'d':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["and"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["and"]) []
lexeraux (':':'=':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++[":="]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++[":="]) []
lexeraux ('d':'o':rest) aux auxiliar
                            | auxiliar == "" = lexeraux rest (aux++["do"]) auxiliar
                            | otherwise = lexeraux rest (aux++[auxiliar]++["do"]) []
lexeraux (a:rest) aux auxiliar = lexeraux rest aux (auxiliar++[a])