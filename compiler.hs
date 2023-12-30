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
compB (Equals a b) = compA b ++ compA a ++ [Equ]
compB (LessEq a b) = compA b ++ compA a ++ [Le]
compB (AndComp a b) = compB b ++ compB a ++ [And]
compB (NegComp a) = compB a ++ [Neg]
compB (EqualsBool a b) = compB b ++ compB a ++ [Equ]
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
parseaux (a:":=":rest) stm = let x = getjustvalue (elemIndex ";" (a:":=":rest))
                              in case parseSumOrProdOrIntOrPar (drop 2 (take (x-1) (a:":=":rest))) of
                                Just (expr,[]) -> parseaux (drop x (a:":=":rest)) (stm++[Assign a expr])
                                Nothing -> error "Parse Error"
                                _ -> error "Parse Error"
parseaux ("(":rest) stm = parseaux (drop (getjustvalue (elemIndex ")" ("(":rest))) ("(":rest)) (stm++parseaux (drop 1 (take (getjustvalue (elemIndex ")" ("(":rest))-1) ("(":rest))) [])
parseaux (";":rest) stm = parseaux rest stm
parseaux ("if":rest) stm = let thenpos = getjustvalue (elemIndex "then" ("if":rest))
                               elsepos = getjustvalue (elemIndex "else" ("if":rest))
                               arrayafter = drop elsepos ("if":rest)
                            in case takefirstelement arrayafter of
                              "(" -> parseaux (drop (getjustvalue (elemIndex ")" arrayafter)) arrayafter) (stm++[BranchS (getJustvalueBexp (parseAndandBoolEq (checkifPar (drop 1 (take (thenpos-1) ("if":rest)))))) (parseaux (drop thenpos (take (elsepos-1) ("if":rest))) []) (parseaux (take (getjustvalue (elemIndex ")" arrayafter)) arrayafter ) [] )])
                              _  -> parseaux (drop (getjustvalue (elemIndex ";" arrayafter)) arrayafter) (stm++[BranchS (getJustvalueBexp (parseAndandBoolEq (checkifPar (drop 1 (take (thenpos-1) ("if":rest)))))) (parseaux (drop thenpos (take (elsepos-1) ("if":rest))) []) (parseaux (take (getjustvalue (elemIndex ";" arrayafter)) arrayafter ) [] )])
parseaux ("while":rest) stm = let dopos = getjustvalue (elemIndex "do" ("while":rest))
                                  arrayafter = drop dopos ("while":rest)
                              in case takefirstelement arrayafter of
                                "(" -> parseaux (drop (getjustvalue (elemIndex ")" arrayafter)) arrayafter) (stm++[LoopS (getJustvalueBexp (parseAndandBoolEq (checkifPar (drop 1 (take (dopos-1) ("while":rest)))))) (parseaux (take (getjustvalue (elemIndex ")" arrayafter)) arrayafter ) [] )])
                                _ -> parseaux (drop (getjustvalue (elemIndex ";" arrayafter)) arrayafter) (stm++[LoopS (getJustvalueBexp (parseAndandBoolEq (checkifPar (drop 1 (take (dopos-1) ("while":rest)))))) (parseaux (take (getjustvalue (elemIndex ";" arrayafter)) arrayafter ) [] )])


getJustvalueBexp :: Maybe (Bexp,[String]) -> Bexp
getJustvalueBexp (Just (a,[")"])) = a
getJustvalueBexp (Just (a,[])) = a
getJustvalueBexp Nothing = error "Parse Error"

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
    Just (expr1,"*":restString1) ->
      case parseProdOrInt restString1 of
        Just (expr2,restString2) ->
          Just (MulComp expr1 expr2,restString2)
        Nothing                  -> Nothing
    result -> result

parseSumOrProdOrInt :: [String] -> Maybe(Aexp,[String])
parseSumOrProdOrInt str =
  case parseProdOrInt str of
    Just (expr1,"+":restString1) ->
      case parseSumOrProdOrInt restString1 of
        Just (expr2,restString2) ->
          Just (AddComp expr1 expr2,restString2)
        Nothing                  -> Nothing
    Just (expr1,"-":restString1) ->
      case parseSumOrProdOrInt restString1 of
        Just (expr2,restString2) ->
          Just (SubComp expr1 expr2,restString2)
        Nothing                  -> Nothing
    result -> result

parseIntOrParentExpr :: [String] -> Maybe (Aexp,[String])
parseIntOrParentExpr ("(":rest) =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr,")":restString1) -> Just (expr,restString1)
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
    Just (expr1,"*":restString1) ->
      case parseProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (MulComp expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseSumOrProdOrIntOrPar :: [String] -> Maybe (Aexp,[String])
parseSumOrProdOrIntOrPar rest =
  case parseProdOrIntOrPar rest of
    Just (expr1,"+":restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (AddComp expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,"-":restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (SubComp expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

------------- PARSE Bexp ----------------

parseLessOrEqOrTrueOrFalseOrParentOrArith :: [String] -> Maybe (Bexp,[String])
parseLessOrEqOrTrueOrFalseOrParentOrArith ("(":rest) =
  case parseAndandBoolEq rest of
    Just (expr,")":restString1) -> Just (expr,restString1)
    Just _ -> Nothing
    Nothing -> Nothing
parseLessOrEqOrTrueOrFalseOrParentOrArith ("True":rest) = Just (TrueComp,rest)
parseLessOrEqOrTrueOrFalseOrParentOrArith ("False":rest) = Just (FalseComp,rest)
parseLessOrEqOrTrueOrFalseOrParentOrArith rest =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr1,"<=":restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) ->
          Just (LessEq expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,"==":restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) ->
          Just (Equals expr1 expr2, restString2)
        Nothing -> Nothing
    result -> Nothing

parseNegAndLessAndEq :: [String] -> Maybe(Bexp, [String])
parseNegAndLessAndEq ("not":rest) =
    case parseLessOrEqOrTrueOrFalseOrParentOrArith rest of
      Just (expr1,restString1) ->
        Just (NegComp expr1,restString1)
      result -> result
parseNegAndLessAndEq rest = parseLessOrEqOrTrueOrFalseOrParentOrArith rest

parseBoolEqAndNeg :: [String] -> Maybe(Bexp, [String])
parseBoolEqAndNeg rest =
  case parseNegAndLessAndEq rest of
    Just (expr1, "=":restString1) ->
      case parseBoolEqAndNeg restString1 of
        Just (expr2, restString2) ->
          Just (EqualsBool expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseAndandBoolEq :: [String] -> Maybe(Bexp,[String])
parseAndandBoolEq rest =
  case parseBoolEqAndNeg rest of
    Just (expr1, "and":restString1) ->
      case parseAndandBoolEq restString1 of
        Just (expr2, restString2) ->
          Just (AndComp expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result


-----------------------------------------

getjustvalue :: Num a => Maybe a -> a
getjustvalue (Just a) = a+1

lexer :: String -> [String]
lexer string = lexeracc string [] []

lexeracc :: String -> [String] -> String -> [String]
lexeracc [] acc stracc | stracc == "" =  acc
                       | otherwise = acc++[stracc]
lexeracc ('w':'h':'i':'l':'e':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["while"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["while"]) []
lexeracc (' ':rest) acc stracc
                            | stracc == "" = lexeracc rest acc []
                            | otherwise = lexeracc rest (acc++[stracc]) []
lexeracc ('i':'f':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["if"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["if"]) []
lexeracc ('t':'h':'e':'n':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["then"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["then"]) []
lexeracc ('e':'l':'s':'e':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["else"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["else"]) []
lexeracc ('*':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["*"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["*"]) []
lexeracc ('+':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["+"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["+"]) []
lexeracc ('/':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["/"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["/"]) []
lexeracc ('-':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["-"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["-"]) []
lexeracc (';':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++[";"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++[";"]) []
lexeracc ('(':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["("]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["("]) []
lexeracc (')':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++[")"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++[")"]) []
lexeracc ('<':'=':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["<="]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["<="]) []
lexeracc ('=':'=':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["=="]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["=="]) []
lexeracc ('n':'o':'t':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["not"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["not"]) []
lexeracc ('=':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["="]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["="]) []
lexeracc ('a':'n':'d':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["and"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["and"]) []
lexeracc (':':'=':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++[":="]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++[":="]) []
lexeracc ('d':'o':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["do"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["do"]) []
lexeracc (a:rest) acc stracc = lexeracc rest acc (stracc++[a])