module SpaceMan.Machine.InputOutput (eval) where

import Data.Char

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> InputOutputOperation ->  IO WhitespaceMachine

eval m PrintCharacter = do
  putChar (chr (fromInteger (head (stack m))))
  return m { stack = tail (stack m),
             pc = (pc m) + 1 }
