module SpaceMan.Program (dumpit, dumpitRaw, runit) where

import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Interpreter
import SpaceMan.Language
import SpaceMan.Parser
import SpaceMan.Transform

type Process = WhitespaceProgram -> IO ()
type ParseResult = Either ParserError WhitespaceProgram

process :: Process -> ParseResult -> IO ()
process _ (Left msg) = putStrLn $ errorBundlePretty msg
process p (Right prg) = p prg

readParseProcess :: Process -> String -> IO ()
readParseProcess p f = do
  content <- readFile f
  process p $ parse whitespaceRead f content

dumpProgram :: Process
dumpProgram [] = return ()
dumpProgram (p:ps) = do
  print p
  dumpProgram ps

runit :: String -> IO ()
runit = readParseProcess (run . load)

dumpit :: String -> IO ()
dumpit = readParseProcess (dumpProgram . labelNames)

dumpitRaw :: String -> IO ()
dumpitRaw = readParseProcess dumpProgram
