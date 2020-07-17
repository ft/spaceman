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

prefix :: [Char] -> Parser ()
prefix [] = return ()
prefix (x:xs) = do
  whitespaceOperator x
  prefix xs

operation :: [Char] -> a -> Parser a
operation xs op = do
  void $ prefix xs
  return op

arithmeticParser :: Parser ArithmeticOperation
arithmeticParser = do
  (      try $ prefix    [ horiztab,   asciispace ])
  (      try $ operation [ asciispace, asciispace ] Add)
    <|> (try $ operation [ asciispace, horiztab   ] Subtract)
    <|> (try $ operation [ asciispace, linefeed   ] Multiply)
    <|> (try $ operation [ horiztab,   asciispace ] Divide)
    <|> (try $ operation [ horiztab,   horiztab   ] Modulo)

ioParser :: Parser InputOutputOperation
ioParser = do
  (      try $ prefix    [ horiztab,   linefeed   ])
  (      try $ operation [ asciispace, asciispace ] PrintCharacter)
    <|> (try $ operation [ asciispace, horiztab   ] PrintNumber)
    <|> (try $ operation [ horiztab,   asciispace ] ReadCharacter)
    <|> (try $ operation [ horiztab,   horiztab   ] ReadNumber)

whitespaceParser :: Parser WhitespaceExpression
whitespaceParser =
  (Arithmetic <$> arithmeticParser) <|> (InputOutput <$> ioParser)


whitespaceRead :: Parser [WhitespaceExpression]
whitespaceRead = do
  discardOthers
  many whitespaceParser

main :: IO ()
main = putStrLn "SpaceMan - A Whitespace implementation"
