module SpaceMan.Machine.Arithmetic (eval) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

arith :: WhitespaceMachine -> (Integer -> Integer -> Integer) -> IO WhitespaceMachine
arith m op = return $ pci $ psh [l `op` r] $ drp 1 m
  where [r,l] = peek 2 m

eval :: WhitespaceMachine -> ArithmeticOperation -> IO WhitespaceMachine

eval m Add      = arith m (+)
eval m Subtract = arith m (-)
eval m Multiply = arith m (*)
eval m Divide   = arith m div
eval m Modulo   = arith m mod
