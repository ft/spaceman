module SpaceMan.Machine.InputOutput (eval) where

import Data.Char
import System.IO

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Tools
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> InputOutputOperation ->  IO WhitespaceMachine

eval m PrintCharacter = do
  putChar (chr (fromInteger (head (stack m))))
  hFlush stdout
  return $ pci $ drp 1 m

eval m PrintNumber = do
  putStr $ show $ head $ stack m
  hFlush stdout
  return $ pci $ drp 1 m

eval m ReadCharacter = do
  c <- getChar
  return $ pci $ psh [toInteger $ ord c] m

eval m ReadNumber = do
  c <- getLine
  return $ pci $ psh [read $ c :: Value] m
