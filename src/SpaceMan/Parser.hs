{-# LANGUAGE TypeFamilies #-}

module SpaceMan.Parser (Parser, ParserError,
                        discardOthers,
                        whitespaceInteger,
                        whitespaceLabel,
                        whitespaceOperator,
                        tag,
                        imp,
                        number,
                        operation)
where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified SpaceMan.Alphabet as WS
import SpaceMan.AbstractSyntaxTree

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

char2bit :: Char -> IntegerBit
char2bit c = if c == WS.space
             then Zero
             else One

bitToInteger :: IntegerBit -> Integer
bitToInteger Zero = 0
bitToInteger One = 1

listToInteger'' :: Integer -> Int -> IntegerString -> Integer
listToInteger'' acc _ [] = acc
listToInteger'' acc bit (x:xs) =
  listToInteger'' (acc + (bitToInteger x * 2 ^ (bit - 1))) (bit - 1) xs

listToInteger' :: IntegerString -> Integer
listToInteger' xs = listToInteger'' 0 (length xs) xs

listToInteger :: IntegerString -> Integer
listToInteger    []  = 0
listToInteger (_:[]) = 0
listToInteger (Zero:ns) = listToInteger'  ns
listToInteger (One:ns) = -1 * listToInteger'  ns

spaceTabString :: Parser IntegerString
spaceTabString = do
  raw <- takeWhileP Nothing notLF
  return $ map char2bit $ filter WS.fromIntegerAlphabet raw
  where notLF = not . (== WS.linefeed)

whitespaceInteger :: Parser Integer
whitespaceInteger = listToInteger <$> spaceTabString

bit2char :: IntegerBit -> Char
bit2char c = if c == Zero
             then 's'
             else 'T'

whitespaceLabel :: Parser Label
whitespaceLabel = Name <$> map bit2char <$> spaceTabString

number :: String -> Parser Integer
number pat = do
  imp pat <?> "[int op: start-of-integer]"
  ns <- whitespaceInteger
  imp [ WS.linefeed ] <?> "[int <linefeed>: end-of-integer]"
  return ns

tag :: String -> Parser Label
tag xs = do
  void $ imp xs
  l <- whitespaceLabel
  imp [ WS.linefeed ] <?> "<linefeed> [end-of-label]"
  return l

-- Instruction Manipulation Parameter
imp :: String -> Parser ()
imp [] = return ()
imp (x:xs) = do
  whitespaceOperator x
  imp xs

operation :: String -> a -> Parser a
operation xs op = do
  void $ imp xs
  return op
