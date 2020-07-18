module SpaceMan.AbstractSyntaxTree where

type Label = String

data StackOperation = Push Integer
                    | Duplicate
                    | Swap
                    | Drop
  deriving (Show, Eq)

data ArithmeticOperation = Add
                         | Subtract
                         | Multiply
                         | Divide
                         | Modulo
  deriving (Show, Eq)

data HeapOperation = Store | Fetch deriving (Show, Eq)

data FlowControlOperation = Mark Label
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
