module SpaceMan.Machine.FlowControl (eval) where

import System.Exit

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

type Predicate = (Value -> Bool)

jmp :: WhitespaceMachine -> Label -> Predicate -> IO WhitespaceMachine
jmp m tag p = return $ pcl n $ drp 1 m
  where a = lbl tag m
        [v] = peek 1 m
        n = if (p v) then a else pcn m

eval :: WhitespaceMachine -> FlowControlOperation -> IO WhitespaceMachine
eval m (Tag _)              = return $ pci m
eval m (Call tag)           = return $ pcl a $ csp m  where a = lbl tag m
eval m (Jump tag)           = return $ pcl a m        where a = lbl tag m
eval m (JumpIfZero tag)     = jmp m tag (== 0)
eval m (JumpIfNegative tag) = jmp m tag (<  0)
eval m Return               = return $ pcl a $ csd m  where a = 1 + (csa m)
eval _ ExitFromProgram      = exitSuccess
