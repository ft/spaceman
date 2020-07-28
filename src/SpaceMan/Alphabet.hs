module SpaceMan.Alphabet where
import Data.Char

linefeed, space, tabular :: Char
linefeed = Data.Char.chr 10
space    = Data.Char.chr 32
tabular  = Data.Char.chr  9

whitespaceAlphabet :: [Char]
whitespaceAlphabet = [ linefeed, space, tabular ]

fromWhitespaceAlphabet :: Char -> Bool
fromWhitespaceAlphabet c = elem c whitespaceAlphabet

integerAlphabet :: [Char]
integerAlphabet = [ space, tabular ]

fromIntegerAlphabet :: Char -> Bool
fromIntegerAlphabet c = elem c integerAlphabet
