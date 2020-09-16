module SpaceMan.Program (dumpit, dumpitRaw, runit) where

import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Interpreter
import SpaceMan.Language
import SpaceMan.Parser
import SpaceMan.Transform

type Printer = WhitespaceExpression -> IO ()
type Process = WhitespaceProgram -> IO ()
type ParseResult = Either ParserError WhitespaceProgram

process :: Process -> ParseResult -> IO ()
process _ (Left msg) = putStrLn $ errorBundlePretty msg
process p (Right prg) = p prg

readParseProcess :: Process -> String -> IO ()
readParseProcess p f = do
  content <- readFile f
  process p $ parse whitespaceRead f content

runit :: String -> IO ()
runit = readParseProcess (run . load)

pp :: String -> IO ()
pp s = putStr $ "  " ++ s

ppfc :: String -> String -> IO ()
ppfc op tag = pp $ "FlowControl $ " ++ op ++ " $ whitespaceLabel " ++ human
  where human = show $ asciiName tag

pretty :: Printer
pretty (FlowControl (Tag tag))            = ppfc "Tag" tag
pretty (FlowControl (Call tag))           = ppfc "Call" tag
pretty (FlowControl (Jump tag))           = ppfc "Jump" tag
pretty (FlowControl (JumpIfZero tag))     = ppfc "JumpIfZero" tag
pretty (FlowControl (JumpIfNegative tag)) = ppfc "JumpIfNegative" tag
pretty e = indented e

indented :: Printer
indented e = pp $ show e

header, footer :: IO ()

header = do
  putStrLn "import SpaceMan.AbstractSyntaxTree"
  putStrLn "import SpaceMan.Generate"
  putStrLn ""
  putStrLn "spaceProgram :: WhitespaceProgram"
  putStrLn "spaceProgram = ["

footer = do
  putStrLn " ]"
  putStrLn ""
  putStrLn "main :: IO ()"
  putStrLn "main = putStr $ generate spaceProgram"

printExpr :: WhitespaceExpression -> Printer -> Integer -> Integer -> IO ()
printExpr e p 1 _             = do header
                                   p e
printExpr e p i m | i == m    = do putStrLn ","
                                   p e
                                   footer
printExpr e p i m | otherwise = do putStrLn ","
                                   p e

dumpProgram :: Printer -> Integer -> Integer -> Process
dumpProgram print _ _ []     = return ()
dumpProgram print i m (p:ps) = do printExpr p print i m
                                  dumpProgram print (i+1) m ps

dumpProgram' :: Printer -> Process
dumpProgram' print p = dumpProgram print 1 (toInteger $ length p) p

dumpit :: String -> IO ()
dumpit p = readParseProcess (dumpProgram' pretty) p

dumpitRaw :: String -> IO ()
dumpitRaw p = readParseProcess (dumpProgram' indented) p
