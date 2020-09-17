module SpaceMan.Generate (generate, whitespaceLabel) where

import Data.Bits
import Data.Char

import SpaceMan.AbstractSyntaxTree
import qualified SpaceMan.Alphabet as WS
import qualified SpaceMan.Encoding as EN

bits :: Integer -> String -> String
bits 0 s = s
bits n s = bits next $ bit : s
  where lsb = n .&. 1
        bit = if lsb == 1 then WS.tabular else WS.space
        next = shiftR n 1

genInt :: Integer -> String
genInt n = sign : bits an []
  where an = abs n
        sign = if n < 0 then WS.tabular else WS.space
        size = bits an

letters :: String -> String -> String
letters [] s = s
letters (x:xs) s = letters xs $ s ++ [ letter ]
  where letter = if isLower x then WS.space else WS.tabular

genTag :: String -> String
genTag t = letters t []

op2string :: WhitespaceExpression -> String
op2string (StackManipulation s) = EN.stack       ++ stackGen s
op2string (Arithmetic a)        = EN.arithmetic  ++ mathGen a
op2string (HeapAccess h)        = EN.heap        ++ heapGen h
op2string (FlowControl f)       = EN.flowControl ++ flowGen f
op2string (InputOutput io)      = EN.io          ++ ioGen io

stackGen :: StackOperation -> String
stackGen (Push n)  = EN.push  ++ genInt n ++ [ WS.linefeed ]
stackGen Duplicate = EN.duplicate
stackGen Swap      = EN.swap
stackGen Drop      = EN.drop
stackGen (Copy n)  = EN.copy  ++ genInt n ++ [ WS.linefeed ]
stackGen (Slide n) = EN.slide ++ genInt n ++ [ WS.linefeed ]

mathGen :: ArithmeticOperation -> String
mathGen Add      = EN.add
mathGen Subtract = EN.subtract
mathGen Multiply = EN.multiply
mathGen Divide   = EN.divide
mathGen Modulo   = EN.modulo

heapGen :: HeapOperation -> String
heapGen Store = EN.store
heapGen Fetch = EN.fetch

flowGen :: FlowControlOperation -> String
flowGen (Tag tag)            = EN.tag        ++ genTag tag ++ [ WS.linefeed ]
flowGen (Call tag)           = EN.call       ++ genTag tag ++ [ WS.linefeed ]
flowGen (Jump tag)           = EN.jump       ++ genTag tag ++ [ WS.linefeed ]
flowGen (JumpIfZero tag)     = EN.jumpIfZero ++ genTag tag ++ [ WS.linefeed ]
flowGen (JumpIfNegative tag) = EN.jumpIfNeg  ++ genTag tag ++ [ WS.linefeed ]
flowGen Return               = EN.callReturn
flowGen ExitFromProgram      = EN.exit

ioGen :: InputOutputOperation -> String
ioGen PrintCharacter = EN.printChar
ioGen PrintNumber    = EN.printNum
ioGen ReadCharacter  = EN.readChar
ioGen ReadNumber     = EN.readNum

gen :: WhitespaceProgram -> String -> String
gen [] s = s
gen (op:ps) s = gen ps $ s ++ op2string op

generate :: WhitespaceProgram -> String
generate p = gen p ""

c2b :: Int -> IntegerString -> Int -> IntegerString
c2b _ acc 256 = acc
c2b c acc s = c2b c (bit:acc) $ shiftL s 1
  where bit = if (s .&. c) == 0 then Zero else One

b2w :: IntegerBit -> Char
b2w One  = 'T'
b2w Zero = 's'

c2word :: Char -> String
c2word c = map b2w word
  where word = c2b (ord c) [] 1

whitespaceLabel :: String -> String
whitespaceLabel "" = ""
whitespaceLabel (x:xs) = c2word x ++ whitespaceLabel xs
