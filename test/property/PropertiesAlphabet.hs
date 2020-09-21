module PropertiesAlphabet (nonIntegerCharacters,
                           nonWhitespaceCharacters) where

import Data.Char
import Test.QuickCheck

import SpaceMan.Alphabet

genCharacter p = oneof $ map return [ c | c <- map chr [0..255], p c ]

nonWSChar :: Gen Char
nonWSChar = genCharacter nonWSPred
  where nonWSPred = (not . fromWhitespaceAlphabet)

newtype Comment = Comment String deriving (Show)
instance Arbitrary Comment where
  arbitrary = Comment <$> listOf nonWSChar

nonWhitespaceCharacters :: Comment -> Bool
nonWhitespaceCharacters (Comment s) = filter fromWhitespaceAlphabet s == []

nonIntChar :: Gen Char
nonIntChar = genCharacter nonIntPred
  where nonIntPred = (not . fromIntegerAlphabet)

newtype NonIntChar = NonIntChar String deriving (Show)
instance Arbitrary NonIntChar where
  arbitrary = NonIntChar <$> listOf nonWSChar

nonIntegerCharacters :: Comment -> Bool
nonIntegerCharacters (Comment s) = filter fromIntegerAlphabet s == []
