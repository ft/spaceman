module TestsHumanLabels (testsHumanLabels) where

import Test.Hspec

import SpaceMan.Generate
import SpaceMan.Transform

checkHumanLabelInverse s = it title $ f s `shouldBe` Human s
  where ps = show s
        title = "(asciiName . label) " ++ ps ++ " => " ++ ps
        f = asciiName . label

t :: Bool -> String -> String -> String
t is ab ch = ch ++ " is"
                ++ (if is then "" else " NOT")
                ++ " in the " ++ ab ++ " alphabet"

poo :: String
spoo :: [String]

poo  =   "sTTTssss" ++ "sTTsTTTT" ++ "sTTsTTTT"
spoo = [ "sTTTssss",   "sTTsTTTT",   "sTTsTTTT" ]

gives :: (Show a, Show b) => String -> a -> b -> String
gives f a b = f ++ " " ++ show a ++ " => " ++ show b

testsHumanLabels = hspec $ do
  describe "HumanReadableLabels.Generic" $ do
    it (gives "makeAsciiName"  poo "poo") $ makeAsciiName poo `shouldBe` "poo"
    it (gives         "label" "poo" poo)  $ label "poo"       `shouldBe` poo
    it (gives        "split8"  poo spoo)  $ split8 poo []     `shouldBe` spoo
  describe "HumanReadableLabels.Inverse" $ do
    checkHumanLabelInverse ""
    checkHumanLabelInverse "a"
    checkHumanLabelInverse "print-hello"
