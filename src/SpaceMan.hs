{-# LANGUAGE TypeFamilies #-}

-- Code in whitespace is written as instruction modification parameters
-- followed by operations.

import Control.Monad
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

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

horiztab, linefeed, asciispace :: Char
horiztab   = Data.Char.chr  9
linefeed   = Data.Char.chr 10
asciispace = Data.Char.chr 32

whitespaceCharacters :: [Char]
whitespaceCharacters = [ horiztab, linefeed, asciispace ]

otherCharacters :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
otherCharacters = takeWhileP Nothing (`notElem` whitespaceCharacters)

discardOthers :: Parser ()
discardOthers = do
  void otherCharacters
  return ()

whitespaceOperator :: Char -> Parser ()
whitespaceOperator op = do
  void $ char op
  discardOthers

additionParser :: Parser ArithmeticOperation
additionParser = do
  whitespaceOperator horiztab
  whitespaceOperator asciispace
  whitespaceOperator asciispace
  whitespaceOperator asciispace
  return Add

subtractParser :: Parser ArithmeticOperation
subtractParser = do
  whitespaceOperator horiztab
  whitespaceOperator asciispace
  whitespaceOperator asciispace
  whitespaceOperator horiztab
  return Subtract

multiplyParser :: Parser ArithmeticOperation
multiplyParser = do
  whitespaceOperator horiztab
  whitespaceOperator asciispace
  whitespaceOperator asciispace
  whitespaceOperator linefeed
  return Multiply

divideParser :: Parser ArithmeticOperation
divideParser = do
  whitespaceOperator horiztab
  whitespaceOperator asciispace
  whitespaceOperator horiztab
  whitespaceOperator asciispace
  return Divide

moduloParser :: Parser ArithmeticOperation
moduloParser = do
  whitespaceOperator horiztab
  whitespaceOperator asciispace
  whitespaceOperator horiztab
  whitespaceOperator horiztab
  return Modulo

arithmeticParser :: Parser ArithmeticOperation
arithmeticParser = do
  discardOthers
  try additionParser
    <|> try subtractParser
    <|> try multiplyParser
    <|> try divideParser
    <|> try moduloParser

whitespaceParser :: Parser WhitespaceExpression
whitespaceParser = Arithmetic <$> arithmeticParser

whitespaceRead :: Parser [WhitespaceExpression]
whitespaceRead = do
  discardOthers
  many whitespaceParser

main :: IO ()
main = putStrLn "SpaceMan - A Whitespace implementation"
