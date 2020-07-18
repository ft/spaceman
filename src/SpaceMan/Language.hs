module SpaceMan.Language (whitespaceParser, whitespaceRead) where

import Control.Monad
import Text.Megaparsec

import qualified SpaceMan.Alphabet as WS
import qualified SpaceMan.AbstractSyntaxTree as AST
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

arithmeticParser :: Parser AST.ArithmeticOperation
arithmeticParser = do
  (      try $ imp       [ WS.tabular, WS.space ])
  (      try $ operation [ WS.space,   WS.space    ] AST.Add)
    <|> (try $ operation [ WS.space,   WS.tabular  ] AST.Subtract)
    <|> (try $ operation [ WS.space,   WS.linefeed ] AST.Multiply)
    <|> (try $ operation [ WS.tabular, WS.space    ] AST.Divide)
    <|> (try $ operation [ WS.tabular, WS.tabular  ] AST.Modulo)

ioParser :: Parser AST.InputOutputOperation
ioParser = do
  (      try $ imp       [ WS.tabular, WS.linefeed ])
  (      try $ operation [ WS.space,   WS.space    ] AST.PrintCharacter)
    <|> (try $ operation [ WS.space,   WS.tabular  ] AST.PrintNumber)
    <|> (try $ operation [ WS.tabular, WS.space    ] AST.ReadCharacter)
    <|> (try $ operation [ WS.tabular, WS.tabular  ] AST.ReadNumber)

heapParser :: Parser AST.HeapOperation
heapParser = do
  (      try $ imp       [ WS.tabular, WS.tabular ])
  (      try $ operation [ WS.space   ] AST.Store)
    <|> (try $ operation [ WS.tabular ] AST.Fetch)

whitespaceParser :: Parser AST.WhitespaceExpression
whitespaceParser =
  (    AST.Arithmetic  <$> arithmeticParser)
  <|> (AST.InputOutput <$> ioParser)
  <|> (AST.HeapAccess  <$> heapParser)

whitespaceRead :: Parser [AST.WhitespaceExpression]
whitespaceRead = do
  discardOthers
  many whitespaceParser
