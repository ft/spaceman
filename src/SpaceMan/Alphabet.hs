module SpaceMan.Alphabet where
import Data.Char

linefeed, space, tabular :: Char
linefeed = Data.Char.chr 10
space    = Data.Char.chr 32
tabular  = Data.Char.chr  9

whitespaceAlphabet :: String
whitespaceAlphabet = [ linefeed, space, tabular ]

fromWhitespaceAlphabet :: Char -> Bool
fromWhitespaceAlphabet c = c `elem` whitespaceAlphabet

integerAlphabet :: String
integerAlphabet = [ space, tabular ]

fromIntegerAlphabet :: Char -> Bool
fromIntegerAlphabet c = c `elem` integerAlphabet
