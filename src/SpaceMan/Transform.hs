module SpaceMan.Transform (asciiName,
                           makeAsciiName,
                           split8,
                           Transformed(..)) where

import Data.Char

import SpaceMan.AbstractSyntaxTree

data Transformed a = Human a
                   | Machine a
  deriving (Show, Eq)

eightPackToInt :: String -> Char
eightPackToInt str = chr value
  where step c (i,n) = (i+1, n + (2^i) * (if c == 's' then 0 else 1))
        (_,value) = foldr step (0,0) str

split8 :: String -> [String]-> [String]
split8 [] acc = acc
split8 xs acc = split8 (drop 8 xs) $ acc ++ [take 8 xs]

replaceUnprintable :: Char -> Char
replaceUnprintable c | isPrint c = c
replaceUnprintable _ | otherwise = '_'

makeAsciiName :: Label -> Label
makeAsciiName l = map (replaceUnprintable . eightPackToInt) $ split8 l []

asciiName :: Label -> Transformed Label
asciiName l = if length l `mod` 8 == 0
              then Human $ makeAsciiName l
              else Machine l
