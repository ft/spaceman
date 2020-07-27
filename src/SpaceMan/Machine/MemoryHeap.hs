module SpaceMan.Machine.MemoryHeap (eval) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> HeapOperation -> IO WhitespaceMachine
eval m Store = return $ pci $ drp 2 $ sto a v m  where [v,a] = peek 2 m
eval m Fetch = return $ pci $ psh [v] $ drp 1 m  where [a]   = peek 1 m
                                                       v     = lda a m
