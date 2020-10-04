module SpaceMan.Machine.Tools (
  -- Stack Memory Instructions
  psh, drp, peek, ref,
  -- Heap Memory Instructions
  sto, lda,
  -- Call Stack Instructions
  csp, csd, csa,
  -- Process Counter Instructions
  pci, pcl, pcn, ldi,
  -- Jump Table Instructions
  lbl)
where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types


-- Stack Memory Instructions

-- | Push: Put new elements on top of the stack.
psh :: [Value] -> WhitespaceMachine -> WhitespaceMachine
psh lst m = m { stack = lst ++ stack m }

-- | Drop: Remove the top-most element on the stack
drp :: Integer -> WhitespaceMachine -> WhitespaceMachine
drp n m = m { stack = drop (fromInteger n) $ stack m }

-- | Peek: Read values from stack (does not modify stack)
peek :: Integer -> WhitespaceMachine -> [Value]
peek n m = take (fromInteger n) $ stack m

-- | Ref: Reference item on stack
ref :: Integer -> WhitespaceMachine -> Value
ref i m = stack m !! fromInteger i

-- Heap Memory Instructions

-- | Store: Put a value into a given address of the machine's heap memory
sto :: Address -> Value -> WhitespaceMachine -> WhitespaceMachine
sto a v m = m { heap = heapStore (heap m) a v }

-- | Load: Load a value from an address of the machine's heap memory and return it
lda :: Address -> WhitespaceMachine -> Value
lda a m = heapFetch (heap m) a


-- Call Stack Instructions

-- | CallStackPush: Put new element onto the top of the call-stack
csp :: WhitespaceMachine -> WhitespaceMachine
csp m = m { callStack = pc m:callStack m }

-- | CallStackDrop: Remove top-most element from call-stack
csd :: WhitespaceMachine -> WhitespaceMachine
csd m = m { callStack = tail (callStack m) }

-- | CallStackAddress: Return top-most value from call-stack
csa :: WhitespaceMachine -> Address
csa m = head $ callStack m


-- Process Counter Register Instructions

-- | ProcessCounterNext: Return the next regular process counter value
pcn :: WhitespaceMachine -> Address
pcn m = pc m + 1

-- | ProcessCounterLoad: Load an arbitrary address into process counter
pcl :: Address -> WhitespaceMachine -> WhitespaceMachine
pcl a m = m { pc = a }

-- | ProcessCounterIncrement: Move process counter forward one address
pci :: WhitespaceMachine -> WhitespaceMachine
pci m = pcl (pcn m) m

-- | LoadInstruction: Load instruction from program memory based on PC
ldi :: WhitespaceMachine -> WhitespaceExpression
ldi m = program m !! fromInteger (pc m)


-- Jump Table Instructions

-- | Label: Returns address of given label
lbl :: String -> WhitespaceMachine -> Address
lbl t m = getLabelAddress (jump m) t
