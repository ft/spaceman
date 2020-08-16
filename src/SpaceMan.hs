{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Interpreter
import SpaceMan.Language
import SpaceMan.Parser
import SpaceMan.Transform

data SpacemanArguments = SpacemanArguments
  { dumpAST         :: Bool,
    transformLabels :: Bool,
    fileName        :: String }
  deriving (Show, Data, Typeable)

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
  putStrLn $ show p
  dumpProgram ps

runit :: String -> IO ()
runit = readParseProcess (\p -> run $ load p)

dumpit :: String -> IO ()
dumpit = readParseProcess (\p -> dumpProgram $ labelNames p)

dumpitRaw :: String -> IO ()
dumpitRaw = readParseProcess (\p -> dumpProgram p)

config = cmdArgsMode $ SpacemanArguments {
  dumpAST = False &= explicit
          &= help "Dump Program AST instead of executing"
          &= name "dump-ast"
          &= name "d",
  transformLabels = False &= explicit
                  &= help "Transform Label Names to ASCII"
                  &= name "transform-labels"
                  &= name "t",
  fileName = def
           &= typ "SourceFile"
           &= argPos 0
  } &= summary "SpaceMan v0.1.0 - A Whitespace Implementation"
    &= help    "Execute whitespace source code"
    &= program "spaceman"
    &= helpArg [explicit, name "h", name "help"]

main :: IO ()
main = do
  args <- cmdArgsRun config
  let input = fileName args
      dump = dumpAST args
      trans = transformLabels args
    in
    if dump == True
    then if trans == True then dumpit input
                          else dumpitRaw input
    else runit input
