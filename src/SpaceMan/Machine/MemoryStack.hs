module SpaceMan.Machine.MemoryStack (eval) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> StackOperation -> IO WhitespaceMachine
eval m (Push n)  = return $ pci $ psh [n] m
eval m Duplicate = return $ pci $ psh h m       where h     = peek 1 m
eval m Swap      = return $ pci $ psh [b,a] m   where [a,b] = peek 2 m
eval m Drop      = return $ pci $ drp 1 m
