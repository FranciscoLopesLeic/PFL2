module Compiler where

import DataTypes
import Debug.Trace
import Data.List ( sortBy, elemIndex )
import Data.Ord (comparing)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Utils
import Data.Char (isDigit)

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA :: Aexp -> Code
compA (Num a) = [Push a]
compA (Var a) = [Fetch a]
compA (AddComp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubComp a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MulComp a1 a2) = compA a1 ++ compA a2 ++ [Mult]

-- compB :: Bexp -> Code
compB :: Bexp -> Code
compB TrueComp = [Tru]
compB FalseComp = [Fals]
compB (Equals a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (LessEq a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (Not a) = compB a ++ [Neg]
compB (AndComp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (EqualsBool b1 b2) = compB b1 ++ compB b2 ++ [Equ]



-- compile :: Program -> Code
compile :: Program -> Code
compile stms = concatMap compileStm stms
  where
    compileStm :: Stm -> Code
    compileStm (Assign var aexp) = compA aexp ++ [Store var]
    compileStm (LoopS bexp stms) = [Loop (compB bexp) (compile stms)]
    compileStm (BranchS bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]

parse :: String -> Program
parse str = parseaux (lexer str) []

parseaux :: [String] -> [Stm] -> [Stm]
parseaux [] stms = stms
parseaux (a:":=":rest) stms = let x = valuejust (elemIndex ";" (a:":=":rest))
                                in case parseAnyOperator (drop 2 (take (x-1) (a:":=":rest))) of
                                    Just (expr, []) -> parseaux (drop x (a:":=":rest)) (stms ++ [Assign a expr])
                                    Nothing -> error "Parse Error"
                                    _ -> error "Parse Error"
parseaux ("(":rest) stms = parseaux (drop (valuejust (elemIndex ")" ("(":rest))) ("(":rest)) (stms ++ parseaux (drop 1 (take (valuejust (elemIndex ")" ("(":rest))-1) ("(":rest))) [])
parseaux (";":rest) stms = parseaux rest stms
parseaux ("if":rest) stm = let thenpos = valuejust (elemIndex "then" ("if":rest))
                               elsepos = valuejust (elemIndex "else" ("if":rest))
                               arrayafter = drop elsepos ("if":rest)
                            in case clearfirst arrayafter of
                              "(" -> parseaux (drop (valuejust (elemIndex ")" arrayafter)) arrayafter) (stm ++ [BranchS (getJustBexp (parseAndCompOrEqualsBoolean (isPar (drop 1 (take (thenpos-1) ("if":rest)))))) (parseaux (drop thenpos (take (elsepos-1) ("if":rest))) []) (parseaux (take (valuejust (elemIndex ")" arrayafter)) arrayafter ) [] )])
                              _  -> parseaux (drop (valuejust (elemIndex ";" arrayafter)) arrayafter) (stm ++ [BranchS (getJustBexp (parseAndCompOrEqualsBoolean (isPar (drop 1 (take (thenpos-1) ("if":rest)))))) (parseaux (drop thenpos (take (elsepos-1) ("if":rest))) []) (parseaux (take (valuejust (elemIndex ";" arrayafter)) arrayafter ) [] )])
parseaux ("while":rest) stm = let dopos = valuejust (elemIndex "do" ("while":rest))
                                  arrayafter = drop dopos ("while":rest)
                              in case clearfirst arrayafter of
                                "(" -> parseaux (drop (valuejust (elemIndex ")" arrayafter)) arrayafter) (stm ++ [LoopS (getJustBexp (parseAndCompOrEqualsBoolean (isPar (drop 1 (take (dopos-1) ("while":rest)))))) (parseaux (take (valuejust (elemIndex ")" arrayafter)) arrayafter ) [] )])
                                _ -> parseaux (drop (valuejust (elemIndex ";" arrayafter)) arrayafter) (stm ++ [LoopS (getJustBexp (parseAndCompOrEqualsBoolean (isPar (drop 1 (take (dopos-1) ("while":rest)))))) (parseaux (take (valuejust (elemIndex ";" arrayafter)) arrayafter ) [] )])


getJustBexp :: Maybe (Bexp,[String]) -> Bexp
getJustBexp (Just (a,[")"])) = a
getJustBexp (Just (a,[])) = a
getJustBexp Nothing = error "Parse Error"

isPar :: [String] -> [String]
isPar ("(":rest) = drop 1 (take (length ("(":rest)) ("(":rest))
isPar rest = rest

clearfirst :: [String] -> String
clearfirst ("(":rest) = "("
clearfirst (a:rest) = a

parseInteiros :: [String] -> Maybe (Aexp,[String])
parseInteiros (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseInteiros _ = Nothing

parseInteirosOuProdutos :: [String] -> Maybe(Aexp,[String])
parseInteirosOuProdutos str =
  case parseInteiros str of
    Just (expr1,"*":restString1) ->
      case parseInteirosOuProdutos restString1 of
        Just (expr2,restString2) ->
          Just (MulComp expr1 expr2,restString2)
        Nothing                  -> Nothing
    result -> result

parseSomaOuInteirosOuProdutos :: [String] -> Maybe(Aexp,[String])
parseSomaOuInteirosOuProdutos str =
  case parseInteirosOuProdutos str of
    Just (expr1,"+":restString1) ->
      case parseSomaOuInteirosOuProdutos restString1 of
        Just (expr2,restString2) ->
          Just (AddComp expr1 expr2,restString2)
        Nothing                  -> Nothing
    Just (expr1,"-":restString1) ->
      case parseSomaOuInteirosOuProdutos restString1 of
        Just (expr2,restString2) ->
          Just (SubComp expr1 expr2,restString2)
        Nothing                  -> Nothing
    result -> result

parseInteirosOuParent :: [String] -> Maybe (Aexp,[String])
parseInteirosOuParent ("(":rest) =
  case parseAnyOperator rest of
    Just (expr,")":restString1) -> Just (expr,restString1)
    Just _ -> Nothing
    Nothing -> Nothing
parseInteirosOuParent (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseInteirosOuParent _ = Nothing

parseInteirosOuProdutosOuPar :: [String] -> Maybe (Aexp,[String])
parseInteirosOuProdutosOuPar rest =
  case parseInteirosOuParent rest of
    Just (expr1,"*":restString1) ->
      case parseInteirosOuProdutosOuPar restString1 of
        Just (expr2,restString2) -> Just (MulComp expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseAnyOperator :: [String] -> Maybe (Aexp,[String])
parseAnyOperator rest =
  case parseInteirosOuProdutosOuPar rest of
    Just (expr1,"+":restString1) ->
      case parseAnyOperator restString1 of
        Just (expr2,restString2) -> Just (AddComp expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,"-":restString1) ->
      case parseAnyOperator restString1 of
        Just (expr2,restString2) -> Just (SubComp expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

------------- PARSE Bexp ----------------

parseAnyOperatorB :: [String] -> Maybe (Bexp,[String])
parseAnyOperatorB ("(":rest) =
  case parseAndCompOrEqualsBoolean rest of
    Just (expr,")":restString1) -> Just (expr,restString1)
    Just _ -> Nothing
    Nothing -> Nothing
parseAnyOperatorB ("True":rest) = Just (TrueComp,rest)
parseAnyOperatorB ("False":rest) = Just (FalseComp,rest)
parseAnyOperatorB rest =
  case parseAnyOperator rest of
    Just (expr1,"<=":restString1) ->
      case parseAnyOperator restString1 of
        Just (expr2,restString2) ->
          Just (LessEq expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,"==":restString1) ->
      case parseAnyOperator restString1 of
        Just (expr2,restString2) ->
          Just (Equals expr1 expr2, restString2)
        Nothing -> Nothing
    result -> Nothing

parseNotOrLessEqOrEquals :: [String] -> Maybe(Bexp, [String])
parseNotOrLessEqOrEquals ("not":rest) =
    case parseAnyOperatorB rest of
      Just (expr1,restString1) ->
        Just (Not expr1,restString1)
      result -> result
parseNotOrLessEqOrEquals rest = parseAnyOperatorB rest

parseNotOrEqualsBoolean :: [String] -> Maybe(Bexp, [String])
parseNotOrEqualsBoolean rest =
  case parseNotOrLessEqOrEquals rest of
    Just (expr1, "=":restString1) ->
      case parseNotOrEqualsBoolean restString1 of
        Just (expr2, restString2) ->
          Just (EqualsBool expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseAndCompOrEqualsBoolean :: [String] -> Maybe(Bexp,[String])
parseAndCompOrEqualsBoolean rest =
  case parseNotOrEqualsBoolean rest of
    Just (expr1, "and":restString1) ->
      case parseAndCompOrEqualsBoolean restString1 of
        Just (expr2, restString2) ->
          Just (AndComp expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

valuejust :: Num a => Maybe a -> a
valuejust (Just a) = a+1

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