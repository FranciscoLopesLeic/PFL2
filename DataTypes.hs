module DataTypes where

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackItem = Inteiro Integer | Verdadeiro | Falso deriving (Show, Eq)
type Stack = [StackItem]

type Key = String
type Value = StackItem

type State = [(Key, Value)]

-- .........................................................................................................
-- .........................................................................................................

data Aexp = Const Integer | Var String | AddComp Aexp Aexp | SubComp Aexp Aexp | MulComp Aexp Aexp deriving Show

data Bexp = TrueComp | FalseComp | Not Bexp | AndComp Bexp Bexp | LessEq Aexp Aexp | Equals Aexp Aexp deriving Show

data Stm = Assign String Aexp | If Bexp Stm Stm | While Bexp Stm | NoopStm deriving Show

type Program = [Stm]