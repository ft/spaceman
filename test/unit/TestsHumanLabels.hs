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

gives :: (Show a, Show b) => a -> b -> String
gives a b = show a ++ " => " ++ show b

makeLabel :: String -> String -> String
makeLabel a b = "makeAsciiName " ++ show a ++ " => " ++ show b

testsHumanLabels = hspec $ do
  describe "HumanReadableLabels.Generic" $ do
    it (makeLabel poo "poo") $ makeAsciiName poo `shouldBe` "poo"
    it (poo `gives` spoo)    $ split8 poo []     `shouldBe` spoo
    it (poo `gives` "poo")   $ makeAsciiName poo `shouldBe` "poo"
  describe "HumanReadableLabels.Inverse" $ do
    checkHumanLabelInverse ""
    checkHumanLabelInverse "a"
    checkHumanLabelInverse "print-hello"
