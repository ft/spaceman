module SpaceMan.Interpreter (loadWhitespace, run, WhitespaceMachine(..)) where

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.List
import System.Exit

import SpaceMan.AbstractSyntaxTree

type JumpTable = [(String, Integer)]

getLabelAddress :: JumpTable -> String -> Integer
getLabelAddress [] _ = 0
getLabelAddress ((tag,address):xs) t = if (tag == t)
                                       then address
                                       else getLabelAddress xs t

type Stack = [Integer]
type Address = Integer
type Value = Integer

type HeapDatum = (Address, Value)
type Heap = [HeapDatum]

heapStore :: Heap -> Address -> Value -> Heap
heapStore [] a v = [ (a,v) ]
heapStore ((addr,value):xs) a v = if (addr == a)
                                  then (a,v):xs
                                  else (addr,value) : heapStore xs a v

heapFetch :: Heap -> Address -> Value
heapFetch [] _ = 0
heapFetch ((addr,value):xs) a = if (addr == a) then value
                                               else heapFetch xs a

data WhitespaceMachine = WhitespaceMachine
  {
    stack     :: Stack,
    callStack :: Stack,
    heap      :: Heap,
    pc        :: Integer,
    jump      :: JumpTable,
    program   :: WhitespaceProgram
  } deriving (Show, Eq)

type StartInfo = (Integer, JumpTable, WhitespaceProgram)
type MachineStart = Either String StartInfo

extractPure :: StartInfo -> WhitespaceExpression -> MachineStart
extractPure (idx, jt, ps) (FlowControl (Tag tag)) =
  if tag `elem` (map fst jt)
  then Left $ "Label " ++ tag ++ " already defined."
  else Right (idx, jt ++ [ (tag, idx) ], ps)
extractPure (idx, jt, ps) p = Right (idx + 1, jt, ps ++ [ p ])

extractJumpTable :: WhitespaceProgram -> MachineStart
extractJumpTable = foldM extractPure (0,[],[])

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

loadWhitespace :: WhitespaceProgram -> Either String WhitespaceMachine
loadWhitespace = loadTheMachine . extractJumpTable

run :: Either String WhitespaceMachine -> IO ()
run (Left msg) = putStrLn msg
run (Right mach) = runMachine mach

evalStack :: WhitespaceMachine -> StackOperation -> IO ()

evalStack m (Push n) = do
  --putStrLn $ "Stack Push | " ++ (show (n:(stack m)))
  runMachine m { stack = n:(stack m),
                 pc = (pc m) + 1 }

evalStack m Duplicate = do
  --putStrLn $ "Stack Duplicate | " ++ (show (h:st))
  runMachine m { stack = h:st,
                 pc = (pc m) + 1 }
  where st = stack m
        h = head st

evalStack m Drop = do
  --putStrLn $ "Stack Drop | " ++ (show $ tail (stack m))
  runMachine m { stack = tail (stack m),
                 pc = (pc m) + 1 }

evalStack m e = do
  putStrLn $ "Missing Stack instruction: " ++ (show e)
  exitFailure

evalHeap :: WhitespaceMachine -> HeapOperation -> IO ()

evalHeap m Store = do
  --putStrLn $ "Heap Store " ++ (show address) ++ ": " ++ (show value)
  runMachine m { heap = heapStore (heap m) address value,
                 stack = drop 2 $ stack m,
                 pc = (pc m) + 1 }
  where value = head $ stack m
        address = head $ tail $ stack m

evalHeap m Fetch = do
  --putStrLn $ "Heap Fetch" ++ ": " ++ (show address) ++ " -> " ++ (show value)
  runMachine m { stack = newStack,
                 pc = (pc m) + 1 }
  where st = stack m
        address = head st
        value = heapFetch (heap m) address
        newStack = value:(tail st)

evalFlowControl :: WhitespaceMachine -> FlowControlOperation -> IO ()

evalFlowControl m (Call tag) = do
  --putStrLn $ "Call " ++ tag ++ " (" ++ (show address) ++ ") [" ++ (show $ (program m) !! (fromInteger address)) ++ "]"
  runMachine m { callStack = (pc m):(callStack m),
                 pc = address }
  where address = getLabelAddress (jump m) tag

evalFlowControl m (JumpIfZero tag) = do
  --putStrLn $ "JumpIfZero " ++ tag ++ " (" ++ (show address) ++ ") [" ++ (show $ (program m) !! (fromInteger address)) ++ "]"
  runMachine m { pc = newPC }
  where address = getLabelAddress (jump m) tag
        newPC = if ((head (stack m)) == 0) then address else (pc m) + 1

evalFlowControl m (Jump tag) = do
  --putStrLn $ "Jump " ++ tag ++ " (" ++ (show address) ++ ") [" ++ (show $ (program m) !! (fromInteger address)) ++ "]"
  runMachine m { pc = address }
  where address = getLabelAddress (jump m) tag

evalFlowControl m Return = do
  --putStrLn $ "Return to (" ++ (show address) ++ ") [" ++ (show $ (program m) !! (fromInteger address)) ++ "]"
  runMachine m { callStack = tail (callStack m),
                 pc = address }
  where address = head (callStack m) + 1

evalFlowControl m ExitFromProgram = do
  --putStrLn $ "ExitFromProgram" ++ (show $ heap m)
  exitSuccess

evalFlowControl m e = do
  putStrLn $ "Missing FlowControl instruction: " ++ (show e)
  exitFailure

evalIO :: WhitespaceMachine -> InputOutputOperation -> IO ()

evalIO m PrintCharacter = do
  --putStrLn $ "Print " ++ [ (chr (fromInteger (head (stack m)))) ]
  putChar (chr (fromInteger (head (stack m))))
  runMachine m { stack = tail (stack m),
                 pc = (pc m) + 1 }

evalArithmetic :: WhitespaceMachine -> ArithmeticOperation -> IO ()

evalArithmetic m Add = do
  --putStrLn $ "Add " ++ (show a) ++ " + " ++ (show b) ++ " = " ++ (show $ a + b)
  runMachine m { stack = (a+b):(drop 2 (stack m)),
                 pc = (pc m) + 1 }
  where a = head $ stack m
        b = head $ tail $ stack m
        res = a + b

machineStep :: WhitespaceMachine -> WhitespaceExpression -> IO ()
machineStep m (StackManipulation s) = evalStack m s
machineStep m (Arithmetic a) = evalArithmetic m a
machineStep m (HeapAccess h) = evalHeap m h
machineStep m (FlowControl f) = evalFlowControl m f
machineStep m (InputOutput io) = evalIO m io

runMachine :: WhitespaceMachine -> IO ()
runMachine m = do
  machineStep m instruction
  where instruction = (program m) !! (fromInteger $ pc m)
