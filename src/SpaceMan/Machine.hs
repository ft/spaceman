module SpaceMan.Machine (machineStep, runMachine) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

import qualified SpaceMan.Machine.Arithmetic as Arith
import qualified SpaceMan.Machine.FlowControl as FlowControl
import qualified SpaceMan.Machine.InputOutput as InOut
import qualified SpaceMan.Machine.MemoryHeap as MemHeap
import qualified SpaceMan.Machine.MemoryStack as MemStack

machineStep :: WhitespaceMachine -> WhitespaceExpression -> IO WhitespaceMachine
machineStep m (StackManipulation s) = MemStack.eval m s
machineStep m (Arithmetic a)        = Arith.eval m a
machineStep m (HeapAccess h)        = MemHeap.eval m h
machineStep m (FlowControl f)       = FlowControl.eval m f
machineStep m (InputOutput io)      = InOut.eval m io

runMachine :: WhitespaceMachine -> IO ()
runMachine m = do
  nextState <- machineStep m instruction
  runMachine nextState
  where instruction = ldi m
