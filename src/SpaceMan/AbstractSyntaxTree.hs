module SpaceMan.AbstractSyntaxTree (WhitespaceProgram,
                                    WhitespaceExpression(..),
                                    InputOutputOperation(..),
                                    FlowControlOperation(..),
                                    HeapOperation(..),
                                    StackOperation(..),
                                    ArithmeticOperation(..),
                                    IntegerBit(..),
                                    IntegerString,
                                    Label(..)) where

data IntegerBit = Zero
                | One
  deriving (Show, Eq)

type IntegerString = [IntegerBit]

data Label = Name String
           | Address Integer
  deriving (Show, Eq)

data StackOperation = Push Integer
                    | Duplicate
                    | Copy Integer
                    | Swap
                    | Drop
                    | Slide Integer
  deriving (Show, Eq)

data ArithmeticOperation = Add
                         | Subtract
                         | Multiply
                         | Divide
                         | Modulo
  deriving (Show, Eq)

data HeapOperation = Store
                   | Fetch
  deriving (Show, Eq)

data FlowControlOperation = Tag Label
                          | Call Label
                          | Jump Label
                          | JumpIfZero Label
                          | JumpIfNegative Label
                          | Return
                          | ExitFromProgram
  deriving (Show, Eq)

data InputOutputOperation = PrintCharacter
                          | PrintNumber
                          | ReadCharacter
                          | ReadNumber
  deriving (Show, Eq)

data WhitespaceExpression = StackManipulation StackOperation
                          | Arithmetic ArithmeticOperation
                          | HeapAccess HeapOperation
                          | FlowControl FlowControlOperation
                          | InputOutput InputOutputOperation
  deriving (Show, Eq)

type WhitespaceProgram = [WhitespaceExpression]
