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
  raw <- getChar
  let c = toInteger $ ord raw in
    return $ pci $ sto a c $ drp 1 m    where [a] = peek 1 m

eval m ReadNumber = do
  raw <- getLine
  let n = read $ raw :: Value in
    return $ pci $ sto a n $ drp 1 m    where [a] = peek 1 m
