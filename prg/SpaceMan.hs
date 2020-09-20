{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import SpaceMan.Program
import Paths_spaceman (version)
import Data.Version (showVersion)

data SpacemanArguments = SpacemanArguments
  { dumpAST         :: Bool,
    transformLabels :: Bool,
    fileName        :: String }
  deriving (Show, Data, Typeable)

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
  } &= summary ("SpaceMan v" ++ showVersion version
                             ++ " - A Whitespace Implementation")
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
    if dump
    then if trans then dumpit input
                  else dumpitRaw input
    else runit input
