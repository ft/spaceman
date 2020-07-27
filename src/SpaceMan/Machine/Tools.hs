module SpaceMan.Machine.Tools (csa, csd, csp,
                               drp, lbl, lda, pci, pcl, peek, psh, sto)
where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types

peek :: Integer -> WhitespaceMachine -> [Integer]
peek n m = take (fromInteger n) $ stack m

psh :: [Integer] -> WhitespaceMachine -> WhitespaceMachine
psh lst m = m { stack = lst ++ (stack m) }

drp :: Integer -> WhitespaceMachine -> WhitespaceMachine
drp n m = m { stack = drop (fromInteger n) $ stack m }

pci :: WhitespaceMachine -> WhitespaceMachine
pci m = m { pc = (pc m) + 1 }

pcl :: Integer -> WhitespaceMachine -> WhitespaceMachine
pcl a m = m { pc = a }

lbl :: Label -> WhitespaceMachine -> Integer
lbl t m = getLabelAddress (jump m) t

csd :: WhitespaceMachine -> WhitespaceMachine
csd m = m { callStack = tail (callStack m) }

csp :: WhitespaceMachine -> WhitespaceMachine
csp m = m { callStack = (pc m):(callStack m) }

csa :: WhitespaceMachine -> Integer
csa m = head $ callStack m

sto :: Integer -> Integer -> WhitespaceMachine -> WhitespaceMachine
sto a v m = m { heap = heapStore (heap m) a v }

lda :: Integer -> WhitespaceMachine -> Integer
lda a m = heapFetch (heap m) a
