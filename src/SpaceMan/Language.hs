module SpaceMan.Language (whitespaceParser, whitespaceRead) where

import Control.Monad
import Text.Megaparsec

import qualified SpaceMan.Alphabet as WS
import SpaceMan.AbstractSyntaxTree
import SpaceMan.Parser

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
