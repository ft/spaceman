module SpaceMan.Transform (labelNames) where

import Data.Char

import SpaceMan.AbstractSyntaxTree

eightPackToInt :: String -> Char
eightPackToInt str = chr value
  where step c (i,n) = ((i+1), n + (2^i) * (if (c == 's') then 0 else 1))
        (_,value) = foldr step (0,0) str

split8 :: String -> [String]-> [String]
split8 [] acc = acc
split8 xs acc = split8 (drop 8 xs) $ acc ++ [(take 8 xs)]

makeAsciiName :: Label -> Label
makeAsciiName l = map eightPackToInt $ split8 l [[]]

asciiName :: Label -> Label
asciiName l = if (length l `mod` 8 == 0)
              then filter isPrint $ makeAsciiName l
              else l

transformName :: WhitespaceExpression -> WhitespaceExpression
transformName (FlowControl (Tag tag))            = FlowControl $ Tag            $ asciiName tag
transformName (FlowControl (Call tag))           = FlowControl $ Call           $ asciiName tag
transformName (FlowControl (Jump tag))           = FlowControl $ Jump           $ asciiName tag
transformName (FlowControl (JumpIfZero tag))     = FlowControl $ JumpIfZero     $ asciiName tag
transformName (FlowControl (JumpIfNegative tag)) = FlowControl $ JumpIfNegative $ asciiName tag
transformName e = e

labelNames :: WhitespaceProgram -> WhitespaceProgram
labelNames = map transformName
