module SpaceMan.Machine.Arithmetic (eval) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

type Operator = (Value -> Value -> Value)

arith :: WhitespaceMachine -> Operator -> IO WhitespaceMachine
arith m op = return $ pci $ psh [l `op` r] $ drp 2 m
  where [r,l] = peek 2 m

eval :: WhitespaceMachine -> ArithmeticOperation -> IO WhitespaceMachine
eval m Add      = arith m (+)
eval m Subtract = arith m (-)
eval m Multiply = arith m (*)
eval m Divide   = arith m div
eval m Modulo   = arith m mod
