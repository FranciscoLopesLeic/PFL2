type Code = [String]

compile :: [Stm] -> Code
compile [] = []
compile (s:stms) = compStm s ++ compile stms

compA:: Aexp -> Code
compA (Const n) = ["push " ++ show n]
compA (Var x) = ["fetch " ++ x]
compA (Add a1 a2) = compA a1 ++ compA a2 ++ ["add"]
compA (Sub a1 a2) = compA a1 ++ compA a2 ++ ["sub"]
compA (Mult a1 a2) = compA a1 ++ compA a2 ++ ["mult"]

compB :: Bexp -> Code
compB (BConst b) = ["push " ++ show b]
compB (Equals a1 a2) = compA a1 ++ compA a2 ++ ["eq"]
compB (LessEq a1 a2) = compA a1 ++ compA a2 ++ ["leq"]
compB (Not b) = compB b ++ ["neg"]
compB (And b1 b2) = compB b1 ++ compB b2 ++ ["and"]

compStm :: Stm -> Code
compStm (Assign x a) = compA a ++ ["store " ++ x]
compStm (Seq s1 s2) = compStm s1 ++ compStm s2
compStm (If b s1 s2) = compB b ++ ["branch " ++ (show $ length c1) ++ " " ++ (show $ length c2)] ++ c1 ++ c2
  where c1 = compStm s1
        c2 = compStm s2
compStm (While b s) = ["loop " ++ (show $ length cb) ++ " " ++ (show $ length cs)] ++ cb ++ cs
  where cb = compB b
        cs = compStm s


lexer :: String -> [String]
lexer = words 

parse :: String -> [Stm]
parse = parseStms . lexer

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
    in (Add a1 a2, rest2)
parseAExp (x:"-":rest) = 
    let (a1, rest1) = parseAExp [x]
        (a2, rest2) = parseAExp rest
    in (Sub a1 a2, rest2)
parseAExp (x:"*":rest) = 
    let (a1, rest1) = parseAExp [x]
        (a2, rest2) = parseAExp rest
    in (Mult a1 a2, rest2)
parseAExp (x:rest) = (Var x, rest)


parseBExp :: [String] -> (Bexp, [String])
parseBExp ("True":rest) = (BConst True, rest)
parseBExp ("False":rest) = (BConst False, rest)
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





data Aexp = Const Int
          | Var String
          | Add Aexp Aexp
          | Sub Aexp Aexp
          | Mult Aexp Aexp


data Bexp = BConst Bool
          | Equals Aexp Aexp
          | LessEq Aexp Aexp
          | Not Bexp
          | And Bexp Bexp 

data Stm = Assign String Aexp
         | Seq Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm