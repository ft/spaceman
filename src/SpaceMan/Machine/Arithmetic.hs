module SpaceMan.Machine.Arithmetic (eval) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> ArithmeticOperation -> IO WhitespaceMachine

eval m Add =
  return m { stack = (a+b):(drop 2 (stack m)),
             pc = (pc m) + 1 }
  where a = head $ stack m
        b = head $ tail $ stack m
        res = a + b
