# PFL2

Projeto 2 PFL

T05_G06

up202105090 : Guilherme Cortes Pina Oliveira Santos 
Contribuition: 50%
up202108796 : Francisco Tomás Marques Lopes
Contribuition: 50%

## Introduction

This project is separated in two primary sections. 

The first section focuses on the creation of an interpreter. This machine language operates with configurations comprising a list of instructions (or code), an evaluation stack, and a storage component.The goal is to build an interpreter that can execute a given sequence of instructions on the machine, manipulating the stack and storage according to the defined operations.

In the second section, we shift our focus to a higher level of abstraction with the development of a compiler. The goal lies in translating these constructs into the low-level machine instructions defined in the first part of the project..

## Main approach

### Part 1

We've create the following data types:

```hs
module DataTypes where

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackItem = Inteiro Integer | Booleano Bool deriving (Show, Eq)
type Stack = [StackItem]
type State = [(String, StackItem)]

data Aexp = Num Integer | Var String | AddComp Aexp Aexp | SubComp Aexp Aexp | MulComp Aexp Aexp deriving Show

data Bexp = TrueComp | FalseComp | NegComp Bexp | AndComp Bexp Bexp | LessEq Aexp Aexp | Equals Aexp Aexp | EqualsBool Bexp Bexp deriving Show

data Stm = Assign String Aexp | LoopS Bexp [Stm] | BranchS Bexp [Stm] [Stm] | If Bexp Stm Stm | Seq [Stm] deriving Show

data Simbolo = Keyword String | Operador String | Symbol String | Identifier String | Number String deriving Show

instance Eq Simbolo where
    Keyword a == Keyword b = a == b
    Operador a == Operador b = a == b
    Identifier a == Identifier b = a == b
    Number a == Number b = a == b
    _ == _ = False

type Program = [Stm]
```

*Inst

Our base data type is the Inst data type which represent all possible instructions in the code.



*StackItem

Represents the items that can be in stack. Only two types (Integer and Bool).

*Aexp 

Represents all arithmetic expressions. Can also be a Integer Num or a Var String.

*Bexp

Represents all comparision expressions, it can have a value of or True or False and englobes all possible comparisions such as Negation, And comparision, Less or Equal, Equal for Arithmetic expressions and Equal for boolean expressions.

*Stm 

All possible statements such has Assigning a variable, Loop, Conditional Branch, Ifs. Can also be a misture of a Sequence of Stm

*Other types

type Code = [Inst] - Array of instructions
type Stack = [StackItem] - List of stack elements (lika a program pile)
type State = [(String, StackItem)] - Array of tuples with string and the stack item of that string.
type Program = [Stm] - The representation of the program, array of statements

As we refer in the introduction we had to interprete the language by pushing elements to a stack and store them in a state.

The function responsible for the overall development of the program was the run() function:

```hs
run :: (Code, Stack, State) -> (Code, Stack, State)
```

Which receives a set of instructions (Code) and returns the result of interpreting all the intructions presented int the "Code" 
For that we needed to make a case for every type Inst.

The types (Push, Equ, Le, And, Neg, Tru, False, Add, Mult, Sub, Fetch and Store) are related to stack management operations. Push piles an integer. Equ, Le, And, Neg compare the values on top of the stack and then push the result to the top of the stack. Tru and Fals put their values respectively on the top of the stack. Add, Mult, Sub use the two elements in the top of the stack to perfrom their operations. Fetch piles in the stack the top of the storage (State), where Store does the opposite, stores the top of the stac.

The other types (Branch, Noop and Loop) are related to the program flow and comparing the stack values.

### Part 2

In part 1 we focused more on transform the code to be executed, now we want to compile the code to be an array of instructions.For that we needed to separate in two main functions: Compile and Parse

The Compile function is responsible for the transformation of the an array of statements "Stm" and make them executable intructions.
Inside of that Compile function we divided it to compile each data type that we've created.
The Parse function receives the high level code and transforms it into the array of statements that the compile function reads.

```hs
parse :: String -> Program
```

The parse function uses a lexer that divides all tokens that a program can have
 
 ```hs
auxiliar :: [String] -> [Stm] -> [Stm]
```

This functions is responsible for ensuring the correct reading of ther parses, specially when it sees certain statements, such as:
":=", ";", "(", ")", "if" and "while".

All other parse(something...) function essentially ensure that the code is well parsed. Exameple:

```hs

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
```

The first function receives an array of strings and returns an arithmetic expression and the rest of the code after the parsing of the preivous array of strings.

The function checks if the first token can be interpreted as an Integer. If it can, it creates a Num expression and returns it along with the rest of the tokens.
If it cannot, it treats the token as a variable name and creates a Var expression, again returning it with the remaining tokens.
If there are no tokens to parse, it returns Nothing.

The second function receives the same arguments.

The function first attempts to parse an integer or variable. It then checks if the next token is an asterisk (*), indicating a multiplication operation:
If there is a multiplication, it recursively calls itself to parse the second operand of the multiplication. If this second parsing succeeds, it creates a MulComp expression with both parsed operands and returns this along with the remaining tokens.
If there is no multiplication, it simply returns the result of the initial parseInteiro call. This allows it to handle single numbers or variables as well as multiplication expressions.

The following functions have the same structure, and all culminate in reading arithmetic expressions.

For the boolean expressions we divide the two statements that we're comparing (could be only one) and intrepert the bool simbols associated with them.


### Conclusion

The project was sucessfly assembled and we built an run and compile system capable of reading a programming language.