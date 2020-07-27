import System.Environment (getArgs)
import Text.Megaparsec (parse)

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Interpreter
import SpaceMan.Language
import SpaceMan.Parser

execute :: Either ParserError WhitespaceProgram -> IO ()
execute (Left msg) = putStrLn $ show msg
execute (Right prg) = run machine
  where machine = load prg

runit :: String -> IO ()
runit f = do
  content <- readFile f
  execute $ parse whitespaceRead f content

usage :: IO ()
usage = do
  putStrLn "SpaceMan - A Whitespace implementation"
  putStrLn "usage: spaceman FILE"

main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 1 then usage
                        else runit $ head args
