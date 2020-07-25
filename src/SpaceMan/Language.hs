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

pushIntegerParser :: Parser StackOperation
pushIntegerParser = do
  imp [ WS.space ] <?> "<space>"
  ns <- whitespaceInteger
  imp [ WS.linefeed ] <?> "<linefeed>"
  return $ Push ns

stackParser :: Parser StackOperation
stackParser = do
  (      try $ imp       [ WS.space ])
  (      try $ pushIntegerParser)
    <|> (try $ operation [ WS.linefeed, WS.space    ] Duplicate)
    <|> (try $ operation [ WS.linefeed, WS.tabular  ] Swap)
    <|> (try $ operation [ WS.linefeed, WS.linefeed ] Drop)

tag :: [Char] -> Parser Label
tag xs = do
  void $ imp xs
  l <- whitespaceLabel
  imp [ WS.linefeed ] <?> "<linefeed>"
  return l

flowControlParser :: Parser FlowControlOperation
flowControlParser = do
  (      try $ imp [ WS.linefeed ])
  (      try $ Tag            <$> tag [ WS.space,    WS.space    ])
    <|> (try $ Call           <$> tag [ WS.space,    WS.tabular  ])
    <|> (try $ Jump           <$> tag [ WS.space,    WS.linefeed ])
    <|> (try $ JumpIfZero     <$> tag [ WS.tabular,  WS.space    ])
    <|> (try $ JumpIfNegative <$> tag [ WS.tabular,  WS.tabular  ])
    <|> (try $ operation              [ WS.tabular,  WS.linefeed ] Return)
    <|> (try $ operation              [ WS.linefeed, WS.linefeed ] ExitFromProgram)

whitespaceParser :: Parser WhitespaceExpression
whitespaceParser =
  (    StackManipulation  <$> stackParser)
  <|> (Arithmetic         <$> arithmeticParser)
  <|> (HeapAccess         <$> heapParser)
  <|> (FlowControl        <$> flowControlParser)
  <|> (InputOutput        <$> ioParser)

whitespaceRead :: Parser [WhitespaceExpression]
whitespaceRead = do
  discardOthers
  many whitespaceParser
