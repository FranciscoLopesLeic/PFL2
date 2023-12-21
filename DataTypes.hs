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