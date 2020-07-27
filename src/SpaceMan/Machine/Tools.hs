module SpaceMan.Machine.Tools (
  -- Stack Memory Instructions
  psh, drp, peek,
  -- Heap Memory Instructions
  sto, lda,
  -- Call Stack Instructions
  csp, csd, csa,
  -- Process Counter Instructions
  pci, pcl,
  -- Jump Table Instructions
  lbl)
where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types


-- Stack Memory Instructions

-- Push: Put a new element on top of the stack.
psh :: [Integer] -> WhitespaceMachine -> WhitespaceMachine
psh lst m = m { stack = lst ++ (stack m) }

-- Drop: Remove the top-most element on the stack
drp :: Integer -> WhitespaceMachine -> WhitespaceMachine
drp n m = m { stack = drop (fromInteger n) $ stack m }

-- Peek: Read values from stack (does not modify stack)
peek :: Integer -> WhitespaceMachine -> [Integer]
peek n m = take (fromInteger n) $ stack m


-- Heap Memory Instructions

-- Store: Put a value into a given address of the machine's heap memory
sto :: Integer -> Integer -> WhitespaceMachine -> WhitespaceMachine
sto a v m = m { heap = heapStore (heap m) a v }

-- Load: Load a value from an address of the machine's heap memory onto stack
lda :: Integer -> WhitespaceMachine -> Integer
lda a m = heapFetch (heap m) a


-- Call Stack Instructions

-- CallStackPush: Put new element onto the top of the call-stack
csp :: WhitespaceMachine -> WhitespaceMachine
csp m = m { callStack = (pc m):(callStack m) }

-- CallStackDrop: Remove top-most element from call-stack
csd :: WhitespaceMachine -> WhitespaceMachine
csd m = m { callStack = tail (callStack m) }

-- CallStackAddress: Return top-most value from call-stack
csa :: WhitespaceMachine -> Integer
csa m = head $ callStack m


-- Process Counter Register Instructions

-- ProcessCounterIncrement: Move process counter forward one address
pci :: WhitespaceMachine -> WhitespaceMachine
pci m = m { pc = (pc m) + 1 }

-- ProcessCounterLoad: Load an arbitrary address into process counter
pcl :: Integer -> WhitespaceMachine -> WhitespaceMachine
pcl a m = m { pc = a }


-- Jump Table Instructions

-- Label: Returns address of given label
lbl :: Label -> WhitespaceMachine -> Integer
lbl t m = getLabelAddress (jump m) t
