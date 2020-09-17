import Data.Char

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Generate

char :: Char -> Integer
char = toInteger . ord

main :: IO ()
main =
  putStr $ generate [
    -- Load "Hello World!" onto the heap.
    StackManipulation (Push 0),
    StackManipulation $ Push $ char 'H',
    HeapAccess Store,
    StackManipulation (Push 1),
    StackManipulation $ Push $ char 'e',
    HeapAccess Store,
    StackManipulation (Push 2),
    StackManipulation $ Push $ char 'l',
    HeapAccess Store,
    StackManipulation (Push 3),
    StackManipulation $ Push $ char 'l',
    HeapAccess Store,
    StackManipulation (Push 4),
    StackManipulation $ Push $ char 'o',
    HeapAccess Store,
    StackManipulation (Push 5),
    StackManipulation $ Push $ char ' ',
    HeapAccess Store,
    StackManipulation (Push 6),
    StackManipulation $ Push $ char 'W',
    HeapAccess Store,
    StackManipulation (Push 7),
    StackManipulation $ Push $ char 'o',
    HeapAccess Store,
    StackManipulation (Push 8),
    StackManipulation $ Push $ char 'r',
    HeapAccess Store,
    StackManipulation (Push 9),
    StackManipulation $ Push $ char 'l',
    HeapAccess Store,
    StackManipulation (Push 10),
    StackManipulation $ Push $ char 'd',
    HeapAccess Store,
    StackManipulation (Push 11),
    StackManipulation $ Push $ char '!',
    HeapAccess Store,
    StackManipulation (Push 12),
    StackManipulation (Push 0),     -- End of String
    HeapAccess Store,

    StackManipulation (Push 0),     -- Loop Variable := 0

    FlowControl $ Call $ label "print-string",
    FlowControl $ Call $ label "print-end-of-line",
    FlowControl ExitFromProgram,


    FlowControl $ Tag $ label "print-string",
    StackManipulation Duplicate,
    HeapAccess Fetch,
    StackManipulation Duplicate,
    FlowControl $ JumpIfZero $ label "label:end-of-string-found",
    InputOutput PrintCharacter,
    StackManipulation (Push 1),
    Arithmetic Add,                 -- Loop Variable + 1
    FlowControl $ Jump $ label "print-string",

    FlowControl $ Tag $ label "label:end-of-string-found",
    StackManipulation Drop,         -- Drop End of String Marker
    StackManipulation Drop,         -- Drop Address Counter
    FlowControl Return,


    FlowControl $ Tag $ label "print-end-of-line",
    StackManipulation (Push 10),
    StackManipulation (Push 13),
    InputOutput PrintCharacter,
    InputOutput PrintCharacter,
    FlowControl Return ]
