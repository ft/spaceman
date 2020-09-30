module SpaceMan.Machine.InputOutput (eval) where

import Data.Char
import System.IO

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> InputOutputOperation ->  IO WhitespaceMachine

eval m PrintCharacter = do
  let [h] = peek 1 m
    in putChar $ chr $ fromInteger h
  hFlush stdout
  return $ pci $ drp 1 m

eval m PrintNumber = do
  let [h] = peek 1 m
    in putStr $ show h
  hFlush stdout
  return $ pci $ drp 1 m

eval m ReadCharacter = do
  raw <- getChar
  let c = toInteger $ ord raw
      [a] = peek 1 m
    in return $ pci $ sto a c $ drp 1 m

eval m ReadNumber = do
  raw <- getLine
  let n = read $ raw :: Value
      [a] = peek 1 m
    in return $ pci $ sto a n $ drp 1 m
