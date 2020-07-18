{-# LANGUAGE TypeFamilies #-}

module SpaceMan.Parser (Parser, ParserError,
                        discardOthers, whitespaceOperator) where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified SpaceMan.Alphabet as WS

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
