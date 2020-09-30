module SpaceMan.Language (whitespaceParser, whitespaceRead) where

import Control.Monad
import Text.Megaparsec

import qualified SpaceMan.Encoding as EN
import SpaceMan.AbstractSyntaxTree
import SpaceMan.Parser

whitespaceRead :: Parser WhitespaceProgram
whitespaceRead = do
  discardOthers
  many whitespaceParser

whitespaceParser :: Parser WhitespaceExpression
whitespaceParser =
  (    StackManipulation  <$> stackParser)
  <|> (Arithmetic         <$> arithmeticParser)
  <|> (HeapAccess         <$> heapParser)
  <|> (FlowControl        <$> flowControlParser)
  <|> (InputOutput        <$> ioParser)

stackParser :: Parser StackOperation
stackParser = do
  (      try $ imp EN.stack)
  (      try $ Push  <$> number EN.push)
    <|> (try $ operation        EN.duplicate Duplicate)
    <|> (try $ operation        EN.swap      Swap)
    <|> (try $ operation        EN.drop      Drop)
    <|> (try $ Copy  <$> number EN.copy)
    <|> (try $ Slide <$> number EN.slide)

arithmeticParser :: Parser ArithmeticOperation
arithmeticParser = do
  (      try $ imp       EN.arithmetic)
  (      try $ operation EN.add       Add)
    <|> (try $ operation EN.subtract  Subtract)
    <|> (try $ operation EN.multiply  Multiply)
    <|> (try $ operation EN.divide    Divide)
    <|> (try $ operation EN.modulo    Modulo)

heapParser :: Parser HeapOperation
heapParser = do
  (      try $ imp       EN.heap)
  (      try $ operation EN.store Store)
    <|> (try $ operation EN.fetch Fetch)

flowControlParser :: Parser FlowControlOperation
flowControlParser = do
  (      try $ imp EN.flowControl)
  (      try $ Tag            <$> tag EN.tag)
    <|> (try $ Call           <$> tag EN.call)
    <|> (try $ Jump           <$> tag EN.jump)
    <|> (try $ JumpIfZero     <$> tag EN.jumpIfZero)
    <|> (try $ JumpIfNegative <$> tag EN.jumpIfNeg)
    <|> (try $ operation              EN.callReturn Return)
    <|> (try $ operation              EN.exit       ExitFromProgram)

ioParser :: Parser InputOutputOperation
ioParser = do
  (      try $ imp       EN.io)
  (      try $ operation EN.printChar PrintCharacter)
    <|> (try $ operation EN.printNum  PrintNumber)
    <|> (try $ operation EN.readChar  ReadCharacter)
    <|> (try $ operation EN.readNum   ReadNumber)
