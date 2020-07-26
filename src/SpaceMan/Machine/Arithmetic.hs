module SpaceMan.Machine.Arithmetic (eval) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine.Types

eval :: WhitespaceMachine -> ArithmeticOperation -> IO WhitespaceMachine

eval m Add =
  return m { stack = res:(drop 2 (stack m)),
             pc = (pc m) + 1 }
  where a = head $ tail $ stack m
        b = head $ stack m
        res = a + b

eval m Subtract = do
  return m { stack = res:(drop 2 (stack m)),
             pc = (pc m) + 1 }
  where a = head $ tail $ stack m
        b = head $ stack m
        res = a - b

eval m Multiply =
  return m { stack = res:(drop 2 (stack m)),
             pc = (pc m) + 1 }
  where a = head $ tail $ stack m
        b = head $ stack m
        res = a * b

eval m Divide =
  return m { stack = res:(drop 2 (stack m)),
             pc = (pc m) + 1 }
  where a = head $ tail $ stack m
        b = head $ stack m
        res = a `div` b

eval m Modulo =
  return m { stack = res:(drop 2 (stack m)),
             pc = (pc m) + 1 }
  where a = head $ tail $ stack m
        b = head $ stack m
        res = a `mod` b
