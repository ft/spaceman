module SpaceMan.Machine.InputOutput (eval) where

import Data.Char

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> InputOutputOperation ->  IO WhitespaceMachine

eval m PrintCharacter = do
  putChar (chr (fromInteger (head (stack m))))
  return m { stack = tail (stack m),
             pc = (pc m) + 1 }

eval m PrintNumber = do
  putStr $ show $ head $ stack m
  return m { stack = tail (stack m),
             pc = (pc m) + 1 }

eval m ReadCharacter = do
  c <- getChar
  return m { stack = (toInteger $ ord c):(stack m),
             pc = (pc m) + 1 }

eval m ReadNumber = do
  c <- getLine
  return m { stack = (read $ c :: Integer):(stack m),
             pc = (pc m) + 1 }
