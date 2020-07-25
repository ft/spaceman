{-# LANGUAGE TypeFamilies #-}

module SpaceMan.Parser (Parser, ParserError,
                        discardOthers,
                        whitespaceInteger,
                        whitespaceLabel,
                        whitespaceOperator) where

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
char2bit c = if (c == WS.space) then Zero
             else One

bitToInteger :: IntegerBit -> Integer
bitToInteger Zero = 0
bitToInteger One = 1

listToInteger'' :: Integer -> Int -> IntegerString -> Integer
listToInteger'' acc _ [] = acc
listToInteger'' acc bit (x:xs) =
  listToInteger'' (acc + ((bitToInteger x) * 2 ^ (bit - 1))) (bit - 1) xs

listToInteger' :: IntegerString -> Integer
listToInteger' xs = listToInteger'' 0 (length xs) xs

listToInteger :: IntegerString -> Integer
listToInteger    []  = 0
listToInteger (_:[]) = 0
listToInteger (Zero:ns) = listToInteger'  ns
listToInteger (One:ns) = -1 * listToInteger'  ns

whitespaceInteger :: Parser Integer
whitespaceInteger = listToInteger
                    <$> map char2bit
                    <$> takeWhile1P Nothing WS.fromIntegerAlphabet

char2label :: Char -> Char
char2label c = if (c == WS.space) then 's'
               else 'T'

whitespaceLabel :: Parser Label
whitespaceLabel = map char2label
                  <$> takeWhile1P Nothing WS.fromLabelAlphabet
