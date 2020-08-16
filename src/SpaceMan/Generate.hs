module SpaceMan.Generate (generate) where

import Data.Bits
import Data.Char

import SpaceMan.AbstractSyntaxTree
import qualified SpaceMan.Alphabet as WS

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

stackGen :: StackOperation -> String
stackGen (Push n) = [ WS.space ] ++ genInt n ++ [ WS.linefeed ]
stackGen Duplicate = [ WS.linefeed, WS.space ]
stackGen Swap = [ WS.linefeed, WS.tabular ]
stackGen Drop = [ WS.linefeed, WS.linefeed ]

heapGen :: HeapOperation -> String
heapGen Store = [ WS.space ]
heapGen Fetch = [ WS.tabular ]

flowGen :: FlowControlOperation -> String
flowGen (Tag tag)            = [ WS.space,   WS.space ]    ++ genTag tag ++ [ WS.linefeed ]
flowGen (Call tag)           = [ WS.space,   WS.tabular ]  ++ genTag tag ++ [ WS.linefeed ]
flowGen (Jump tag)           = [ WS.space,   WS.linefeed ] ++ genTag tag ++ [ WS.linefeed ]
flowGen (JumpIfZero tag)     = [ WS.tabular, WS.space ]    ++ genTag tag ++ [ WS.linefeed ]
flowGen (JumpIfNegative tag) = [ WS.tabular, WS.tabular ]  ++ genTag tag ++ [ WS.linefeed ]
flowGen Return               = [ WS.tabular, WS.linefeed ]
flowGen ExitFromProgram      = [ WS.linefeed, WS.linefeed ]

ioGen :: InputOutputOperation -> String
ioGen PrintCharacter = [ WS.space,   WS.space   ]
ioGen PrintNumber    = [ WS.space,   WS.tabular ]
ioGen ReadCharacter  = [ WS.tabular, WS.space   ]
ioGen ReadNumber     = [ WS.tabular, WS.tabular ]

mathGen :: ArithmeticOperation -> String
mathGen Add      = [ WS.space,   WS.space    ]
mathGen Subtract = [ WS.space,   WS.tabular  ]
mathGen Multiply = [ WS.space,   WS.linefeed ]
mathGen Divide   = [ WS.tabular, WS.space    ]
mathGen Modulo   = [ WS.tabular, WS.space    ]

op2string :: WhitespaceExpression -> String
op2string (StackManipulation s) = [ WS.space ]                ++ stackGen s
op2string (HeapAccess h)        = [ WS.tabular, WS.tabular]   ++ heapGen h
op2string (FlowControl f)       = [ WS.linefeed ]             ++ flowGen f
op2string (InputOutput io)      = [ WS.tabular, WS.linefeed ] ++ ioGen io
op2string (Arithmetic a)        = [ WS.tabular, WS.space ]    ++ mathGen a

gen :: WhitespaceProgram -> String -> String
gen [] s = s
gen (op:ps) s = gen ps $ s ++ op2string op

generate :: WhitespaceProgram -> String
generate p = gen p ""
