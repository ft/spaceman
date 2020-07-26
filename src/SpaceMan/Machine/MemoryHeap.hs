module SpaceMan.Machine.MemoryHeap (eval) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> HeapOperation -> IO WhitespaceMachine

eval m Store = do
  return m { heap = heapStore (heap m) address value,
             stack = drop 2 $ stack m,
             pc = (pc m) + 1 }
  where value = head $ stack m
        address = head $ tail $ stack m

eval m Fetch = do
  return m { stack = newStack,
             pc = (pc m) + 1 }
  where st = stack m
        address = head st
        value = heapFetch (heap m) address
        newStack = value:(tail st)
