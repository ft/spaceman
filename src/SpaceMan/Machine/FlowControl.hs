module SpaceMan.Machine.FlowControl (eval) where

import System.Exit

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

type Comparison = (Integer -> Integer -> Bool)

jmp :: WhitespaceMachine -> String -> Comparison -> IO WhitespaceMachine
jmp m tag op = return $ pcl n $ drp 1 m
  where a = lbl tag m
        [v] = peek 1 m
        n = if (v `op` 0)
            then a
            else (pc m) + 1

eval :: WhitespaceMachine -> FlowControlOperation -> IO WhitespaceMachine
eval m (Tag _)              = return $ pci m
eval m (Call tag)           = return $ pcl a $ csp m  where a = lbl tag m
eval m (Jump tag)           = return $ pcl a m        where a = lbl tag m
eval m (JumpIfZero tag)     = jmp m tag (==)
eval m (JumpIfNegative tag) = jmp m tag (<)
eval m Return               = return $ pcl a $ csd m  where a = 1 + (csa m)
eval _ ExitFromProgram      = exitSuccess
