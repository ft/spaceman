module SpaceMan.Generate (generate, label, label') where

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

numbered :: Integer -> String
numbered n = genInt n ++ [ WS.linefeed ]

tagged :: String -> String
tagged t = genTag t ++ [ WS.linefeed ]

op2string :: WhitespaceExpression -> String
op2string (StackManipulation s) = EN.stack       ++ stackGen s
op2string (Arithmetic a)        = EN.arithmetic  ++ mathGen a
op2string (HeapAccess h)        = EN.heap        ++ heapGen h
op2string (FlowControl f)       = EN.flowControl ++ flowGen f
op2string (InputOutput io)      = EN.io          ++ ioGen io

stackGen :: StackOperation -> String
stackGen (Push n)  = EN.push  ++ numbered n
stackGen Duplicate = EN.duplicate
stackGen Swap      = EN.swap
stackGen Drop      = EN.drop
stackGen (Copy n)  = EN.copy  ++ numbered n
stackGen (Slide n) = EN.slide ++ numbered n

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
flowGen (Tag            (Name tag)) = EN.tag        ++ tagged tag
flowGen (Call           (Name tag)) = EN.call       ++ tagged tag
flowGen (Jump           (Name tag)) = EN.jump       ++ tagged tag
flowGen (JumpIfZero     (Name tag)) = EN.jumpIfZero ++ tagged tag
flowGen (JumpIfNegative (Name tag)) = EN.jumpIfNeg  ++ tagged tag
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

label' :: String -> String
label' "" = ""
label' (x:xs) = c2word x ++ label' xs

label :: String -> Label
label s = Name $ label' s
