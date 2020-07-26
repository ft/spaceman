import SpaceMan.AbstractSyntaxTree
import SpaceMan.Generate

main :: IO ()
main = do
  putStr $ generate [
    -- Load Hello World! into the heap.
    StackManipulation (Push 0),
    StackManipulation (Push 72),    -- H
    HeapAccess Store,
    StackManipulation (Push 1),
    StackManipulation (Push 101),   -- e
    HeapAccess Store,
    StackManipulation (Push 2),
    StackManipulation (Push 108),   -- l
    HeapAccess Store,
    StackManipulation (Push 3),
    StackManipulation (Push 108),   -- l
    HeapAccess Store,
    StackManipulation (Push 4),
    StackManipulation (Push 111),   -- o
    HeapAccess Store,
    StackManipulation (Push 5),
    StackManipulation (Push 32),    -- Space
    HeapAccess Store,
    StackManipulation (Push 6),
    StackManipulation (Push 87),    -- W
    HeapAccess Store,
    StackManipulation (Push 7),
    StackManipulation (Push 111),   -- o
    HeapAccess Store,
    StackManipulation (Push 8),
    StackManipulation (Push 114),   -- r
    HeapAccess Store,
    StackManipulation (Push 9),
    StackManipulation (Push 108),   -- l
    HeapAccess Store,
    StackManipulation (Push 10),
    StackManipulation (Push 100),   -- d
    HeapAccess Store,
    StackManipulation (Push 11),
    StackManipulation (Push 33),    -- !
    HeapAccess Store,
    StackManipulation (Push 12),
    StackManipulation (Push 0),     -- End of String
    HeapAccess Store,

    StackManipulation (Push 0),     -- Loop Variable := 0

    FlowControl (Call "sss"),       -- Call PrintString Function
    FlowControl (Call "TTT"),       -- Call PrintEndline Function
    FlowControl ExitFromProgram,


    FlowControl (Tag "sss"),        -- PrintString Function
    StackManipulation Duplicate,
    HeapAccess Fetch,
    FlowControl (JumpIfZero "TsT"), -- If End of String
    InputOutput PrintCharacter,
    StackManipulation (Push 1),
    Arithmetic Add,                 -- Loop Variable + 1
    FlowControl (Jump "sss"),       -- TailRecurse

    FlowControl (Tag "TsT"),        -- Found End of String in Heap
    StackManipulation Drop,         -- Drop End of String Marker
    StackManipulation Drop,         -- Drop Address Counter
    FlowControl Return,


    FlowControl (Tag "TTT"),        -- PrintEndline Function
    StackManipulation (Push 10),
    StackManipulation (Push 13),
    InputOutput PrintCharacter,
    InputOutput PrintCharacter,
    FlowControl Return ]
