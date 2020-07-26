module SpaceMan.Machine.FlowControl (eval) where

import System.Exit

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> FlowControlOperation -> IO WhitespaceMachine

eval m (Call tag) =
  return m { callStack = (pc m):(callStack m),
             pc = address }
  where address = getLabelAddress (jump m) tag

eval m (JumpIfZero tag) =
  return m { stack = tail $ stack m,
             pc = newPC }
  where address = getLabelAddress (jump m) tag
        newPC = if ((head (stack m)) == 0) then address else (pc m) + 1

eval m (Jump tag) =
  return m { pc = address }
  where address = getLabelAddress (jump m) tag

eval m Return =
  return m { callStack = tail (callStack m),
             pc = address }
  where address = head (callStack m) + 1

eval m ExitFromProgram = exitSuccess
