module SpaceMan.Machine.MemoryStack (eval) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> StackOperation -> IO WhitespaceMachine

eval m (Push n) = do
  return m { stack = n:(stack m), pc = (pc m) + 1 }

eval m Duplicate = do
  return m { stack = h:st, pc = (pc m) + 1 }
  where st = stack m
        h = head st

eval m Drop = do
  return m { stack = tail (stack m), pc = (pc m) + 1 }
