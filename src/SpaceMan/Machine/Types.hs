module SpaceMan.Machine.Types (extractJumpTable,
                               resolveLabels,
                               getLabelAddress,
                               heapFetch,
                               heapStore,
                               loadTheMachine,
                               Address, Value,
                               WhitespaceMachine(..)) where

import SpaceMan.AbstractSyntaxTree

import Control.Monad

type JumpTable = [(String, Address)]

getLabelAddress :: JumpTable -> String -> Address
getLabelAddress [] _ = 0
getLabelAddress ((tag,address):xs) t =
  if tag == t then address
              else getLabelAddress xs t

type Stack = [Integer]
type Address = Integer
type Value = Integer

type HeapDatum = (Address, Value)
type Heap = [HeapDatum]

heapStore :: Heap -> Address -> Value -> Heap
heapStore [] a v = [ (a,v) ]
heapStore ((addr,value):xs) a v =
  if addr == a then (a,v):xs
               else (addr,value) : heapStore xs a v

heapFetch :: Heap -> Address -> Value
heapFetch [] _ = 0
heapFetch ((addr,value):xs) a =
  if addr == a then value
               else heapFetch xs a

type StartInfo = (Integer, JumpTable, WhitespaceProgram)
type MachineStart = Either String StartInfo

extractPure :: StartInfo -> WhitespaceExpression -> MachineStart
extractPure (idx, jt, ps) (FlowControl (Tag (Name tag))) =
  if tag `elem` map fst jt
  then Left $ "Label " ++ tag ++ " already defined."
  else Right (idx, jt ++ [ (tag, idx) ], ps)
extractPure (idx, jt, ps) p = Right (idx + 1, jt, ps ++ [ p ])

extractJumpTable :: WhitespaceProgram -> MachineStart
extractJumpTable = foldM extractPure (0,[],[])

resolveLabel :: JumpTable -> WhitespaceExpression -> WhitespaceExpression
resolveLabel jt (FlowControl (Call (Name l))) =
  FlowControl $ Call $ Address $ getLabelAddress jt l
resolveLabel jt (FlowControl (Jump (Name l))) =
  FlowControl $ Jump $ Address $ getLabelAddress jt l
resolveLabel jt (FlowControl (JumpIfZero (Name l))) =
  FlowControl $ JumpIfZero $ Address $ getLabelAddress jt l
resolveLabel jt (FlowControl (JumpIfNegative (Name l))) =
  FlowControl $ JumpIfNegative $ Address $ getLabelAddress jt l
resolveLabel _ op = op

resolveLabels :: MachineStart -> MachineStart
resolveLabels (Left msg) = Left msg
resolveLabels (Right (idx,jt,ps)) = Right (idx, jt, map (resolveLabel jt) ps)

data WhitespaceMachine = WhitespaceMachine
  {
    stack     :: Stack,
    callStack :: Stack,
    heap      :: Heap,
    pc        :: Address,
    jump      :: JumpTable,
    program   :: WhitespaceProgram
  } deriving (Show, Eq)

loadTheMachine :: MachineStart -> Either String WhitespaceMachine
loadTheMachine (Left msg) = Left msg
loadTheMachine (Right (_, jumpTable, reducedProgram)) =
  Right WhitespaceMachine {
    stack = [],
    callStack = [],
    heap = [],
    pc = 0,
    jump = jumpTable,
    program = reducedProgram
  }
