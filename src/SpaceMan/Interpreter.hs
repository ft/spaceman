module SpaceMan.Interpreter (load, run) where

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Machine
import SpaceMan.Machine.Types

load :: WhitespaceProgram -> Either String WhitespaceMachine
load = loadTheMachine . resolveLabels . extractJumpTable

run :: Either String WhitespaceMachine -> IO ()
run (Left msg) = putStrLn msg
run (Right mach) = runMachine mach
