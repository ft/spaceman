module SpaceMan.Machine.FlowControl (eval) where

import System.Exit

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

type Predicate = (Value -> Bool)

jmp' :: WhitespaceMachine -> Integer -> Predicate -> IO WhitespaceMachine
jmp' m a p = return $ pcl n $ drp 1 m
  where [v] = peek 1 m
        n = if p v then a else pcn m

jmp :: WhitespaceMachine -> Label -> Predicate -> IO WhitespaceMachine
jmp m (Address a) p = jmp' m a p
jmp m (Name tag) p  = jmp' m a p
  where a = lbl tag m

eval :: WhitespaceMachine -> FlowControlOperation -> IO WhitespaceMachine
eval m (Tag _)                      = return $ pci m
eval m (Call           (Name tag))  = return $ pcl a $ csp m  where a = lbl tag m
eval m (Call           (Address a)) = return $ pcl a $ csp m
eval m (Jump           (Name tag))  = return $ pcl a m        where a = lbl tag m
eval m (Jump           (Address a)) = return $ pcl a m
eval m (JumpIfZero     tag)         = jmp m tag (== 0)
eval m (JumpIfNegative tag)         = jmp m tag (<  0)
eval m Return                       = return $ pcl a $ csd m  where a = 1 + csa m
eval _ ExitFromProgram              = exitSuccess
