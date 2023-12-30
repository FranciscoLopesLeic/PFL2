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

data Stm = Assign String Aexp | LoopS Bexp [Stm] | BranchS Bexp [Stm] [Stm] deriving Show

type Program = [Stm]