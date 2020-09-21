module TestsLanguage (testsLanguage) where

import Test.Hspec
import Text.Megaparsec as MP

import SpaceMan.AbstractSyntaxTree
import SpaceMan.Alphabet
import SpaceMan.Encoding as EN
import SpaceMan.Language

tl :: String -> WhitespaceExpression -> SpecWith ()
tl input op = it (show op ++ " parses")
            $ (MP.parse whitespaceParser "" input) `shouldBe` (Right op)

nstr :: String
nstr = [ space,
         -- 1111011 := 123
         tabular, tabular, tabular, tabular, space, tabular, tabular,
         linefeed ]

inum :: Integer
inum = 123

tstr :: String
tstr = nstr

fc :: String
fc = EN.flowControl

testsLanguage :: IO ()
testsLanguage = hspec $ do
  describe "Language.Toplevel.Stack" $ do
    tl (EN.stack ++ EN.push ++ nstr)  (StackManipulation $ Push inum)
    tl (EN.stack ++ EN.duplicate)     (StackManipulation Duplicate)
    tl (EN.stack ++ EN.swap)          (StackManipulation Swap)
    tl (EN.stack ++ EN.drop)          (StackManipulation Drop)
    tl (EN.stack ++ EN.copy ++ nstr)  (StackManipulation $ Copy inum)
    tl (EN.stack ++ EN.slide ++ nstr) (StackManipulation $ Slide inum)
  describe "Language.Toplevel.Arithmetic" $ do
    tl (EN.arithmetic ++ EN.add)      (Arithmetic Add)
    tl (EN.arithmetic ++ EN.subtract) (Arithmetic Subtract)
    tl (EN.arithmetic ++ EN.multiply) (Arithmetic Multiply)
    tl (EN.arithmetic ++ EN.divide)   (Arithmetic Divide)
    tl (EN.arithmetic ++ EN.modulo)   (Arithmetic Modulo)
  describe "Language.Toplevel.Heap" $ do
    tl (EN.heap ++ EN.store)          (HeapAccess Store)
    tl (EN.heap ++ EN.fetch)          (HeapAccess Fetch)
  describe "Language.Toplevel.FlowControl" $ do
    tl (fc ++ EN.tag ++ tstr)         (FlowControl $ Tag "sTTTTsTT")
    tl (fc ++ EN.call ++ tstr)        (FlowControl $ Call "sTTTTsTT")
    tl (fc ++ EN.jump ++ tstr)        (FlowControl $ Jump "sTTTTsTT")
    tl (fc ++ EN.jumpIfZero ++ tstr)  (FlowControl $ JumpIfZero "sTTTTsTT")
    tl (fc ++ EN.jumpIfNeg ++ tstr)   (FlowControl $ JumpIfNegative "sTTTTsTT")
    tl (fc ++ EN.callReturn)          (FlowControl $ Return)
    tl (fc ++ EN.exit)                (FlowControl $ ExitFromProgram)
  describe "Language.Toplevel.InputOutput" $ do
    tl (EN.io ++ EN.printChar)        (InputOutput PrintCharacter)
    tl (EN.io ++ EN.printNum)         (InputOutput PrintNumber)
    tl (EN.io ++ EN.readChar)         (InputOutput ReadCharacter)
    tl (EN.io ++ EN.readNum)          (InputOutput ReadNumber)
