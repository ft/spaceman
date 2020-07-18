{-# LANGUAGE TypeFamilies #-}

-- Code in whitespace is written as instruction modification parameters
-- followed by operations.

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified SpaceMan.Alphabet as WS

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

type Parser = Parsec Void String
type ParserError = (ParseErrorBundle String Void)

otherCharacters :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
otherCharacters = takeWhileP Nothing $ not . WS.fromWhitespaceAlphabet

discardOthers :: Parser ()
discardOthers = do
  void otherCharacters
  return ()

whitespaceOperator :: Char -> Parser ()
whitespaceOperator op = do
  void $ char op
  discardOthers

-- Instruction Manipulation Parameter
imp :: [Char] -> Parser ()
imp [] = return ()
imp (x:xs) = do
  whitespaceOperator x
  imp xs

operation :: [Char] -> a -> Parser a
operation xs op = do
  void $ imp xs
  return op

arithmeticParser :: Parser ArithmeticOperation
arithmeticParser = do
  (      try $ imp       [ WS.tabular, WS.space ])
  (      try $ operation [ WS.space,   WS.space    ] Add)
    <|> (try $ operation [ WS.space,   WS.tabular  ] Subtract)
    <|> (try $ operation [ WS.space,   WS.linefeed ] Multiply)
    <|> (try $ operation [ WS.tabular, WS.space    ] Divide)
    <|> (try $ operation [ WS.tabular, WS.tabular  ] Modulo)

ioParser :: Parser InputOutputOperation
ioParser = do
  (      try $ imp       [ WS.tabular, WS.linefeed ])
  (      try $ operation [ WS.space,   WS.space    ] PrintCharacter)
    <|> (try $ operation [ WS.space,   WS.tabular  ] PrintNumber)
    <|> (try $ operation [ WS.tabular, WS.space    ] ReadCharacter)
    <|> (try $ operation [ WS.tabular, WS.tabular  ] ReadNumber)

heapParser :: Parser HeapOperation
heapParser = do
  (      try $ imp       [ WS.tabular, WS.tabular ])
  (      try $ operation [ WS.space   ] Store)
    <|> (try $ operation [ WS.tabular ] Fetch)

whitespaceParser :: Parser WhitespaceExpression
whitespaceParser =
  (    Arithmetic  <$> arithmeticParser)
  <|> (InputOutput <$> ioParser)
  <|> (HeapAccess  <$> heapParser)

whitespaceRead :: Parser [WhitespaceExpression]
whitespaceRead = do
  discardOthers
  many whitespaceParser

main :: IO ()
main = putStrLn "SpaceMan - A Whitespace implementation"
