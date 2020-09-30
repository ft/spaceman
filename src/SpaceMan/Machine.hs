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
machineStep m (StackManipulation e) = MemStack.eval    m e
machineStep m (Arithmetic        e) = Arith.eval       m e
machineStep m (HeapAccess        e) = MemHeap.eval     m e
machineStep m (FlowControl       e) = FlowControl.eval m e
machineStep m (InputOutput       e) = InOut.eval       m e

runMachine :: WhitespaceMachine -> IO ()
runMachine m = runMachine =<< machineStep m instruction
  where instruction = ldi m
