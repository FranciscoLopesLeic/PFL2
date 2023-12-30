module DataTypes where

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackItem = Inteiro Integer | Booleano Bool deriving (Show, Eq)
type Stack = [StackItem]
type State = [(String, StackItem)]

-- .........................................................................................................
-- .........................................................................................................

data Aexp = Num Integer | Var String | AddComp Aexp Aexp | SubComp Aexp Aexp | MulComp Aexp Aexp deriving Show

data Bexp = TrueComp | FalseComp | NegComp Bexp | AndComp Bexp Bexp | LessEq Aexp Aexp | Equals Aexp Aexp | EqualsBool Bexp Bexp deriving Show

data Stm = Assign String Aexp | LoopS Bexp [Stm] | BranchS Bexp [Stm] [Stm] | If Bexp Stm Stm | Seq [Stm] deriving Show

--MUDAR
data Simbolo = Keyword String | Operador String | Symbol String | Identifier String | Number String deriving Show

instance Eq Simbolo where
    Keyword a == Keyword b = a == b
    Operador a == Operador b = a == b
    Identifier a == Identifier b = a == b
    Number a == Number b = a == b
    _ == _ = False

type Program = [Stm]