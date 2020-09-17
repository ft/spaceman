module SpaceMan.Language (whitespaceParser, whitespaceRead) where

import Control.Monad
import Text.Megaparsec

import qualified SpaceMan.Encoding as WS
import SpaceMan.AbstractSyntaxTree
import SpaceMan.Parser

whitespaceRead :: Parser [WhitespaceExpression]
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
  (      try $ imp WS.stack)
  (      try $ Push  <$> number WS.push)
    <|> (try $ operation        WS.duplicate Duplicate)
    <|> (try $ operation        WS.swap      Swap)
    <|> (try $ operation        WS.drop      Drop)
    <|> (try $ Copy  <$> number WS.copy)
    <|> (try $ Slide <$> number WS.slide)

arithmeticParser :: Parser ArithmeticOperation
arithmeticParser = do
  (      try $ imp       WS.artithmetic)
  (      try $ operation WS.add       Add)
    <|> (try $ operation WS.substract Subtract)
    <|> (try $ operation WS.multiply  Multiply)
    <|> (try $ operation WS.divide    Divide)
    <|> (try $ operation WS.modulo    Modulo)

heapParser :: Parser HeapOperation
heapParser = do
  (      try $ imp       WS.heap)
  (      try $ operation WS.store Store)
    <|> (try $ operation WS.fetch Fetch)

flowControlParser :: Parser FlowControlOperation
flowControlParser = do
  (      try $ imp WS.flowControl)
  (      try $ Tag            <$> tag WS.tag)
    <|> (try $ Call           <$> tag WS.call)
    <|> (try $ Jump           <$> tag WS.jump)
    <|> (try $ JumpIfZero     <$> tag WS.jumpIfZero)
    <|> (try $ JumpIfNegative <$> tag WS.jumpIfNeg)
    <|> (try $ operation              WS.callReturn Return)
    <|> (try $ operation              WS.exit       ExitFromProgram)

ioParser :: Parser InputOutputOperation
ioParser = do
  (      try $ imp       WS.io)
  (      try $ operation WS.printChar PrintCharacter)
    <|> (try $ operation WS.printNum  PrintNumber)
    <|> (try $ operation WS.readChar  ReadCharacter)
    <|> (try $ operation WS.readNum   ReadNumber)
