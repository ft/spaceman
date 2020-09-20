import Test.Hspec

import SpaceMan.Alphabet
import SpaceMan.Generate
import SpaceMan.Transform

isInWS :: String -> String
isInWS ch = ch ++ "is in the alphabet"

isInInt :: Bool -> String -> String
isInInt isit ch = ch ++ "is "
                     ++ if isit then "" else " NOT"
                     ++ " in the alphabet"

labelToHuman s =
  it (s ++ " converts back and forth") $ ((asciiName . label) s) `shouldBe` Human s

main :: IO ()
main = hspec $ do
  describe "Alphabet.Characters" $ do
    it "<space> is ' '"  $ space    `shouldBe` ' '
    it "<lf> is '\\n'"   $ linefeed `shouldBe` '\n'
    it "<tab> is '\\t'"  $ tabular  `shouldBe` '\t'
  describe "Alphabet.WhitespaceAlphabet" $ do
    it (isInWS "<space>")       $ (fromWhitespaceAlphabet ' ')  `shouldBe` True
    it (isInWS "<lf>")          $ (fromWhitespaceAlphabet '\n') `shouldBe` True
    it (isInWS "<tab>")         $ (fromWhitespaceAlphabet '\t') `shouldBe` True
  describe "Alphabet.IntegerAlphabet" $ do
    it (isInInt True "<space>") $ (fromIntegerAlphabet ' ')     `shouldBe` True
    it (isInInt False "<lf>")   $ (fromIntegerAlphabet '\n')    `shouldBe` False
    it (isInInt True "<tab>")   $ (fromIntegerAlphabet '\t')    `shouldBe` True
  describe "HumanReadableLabels" $ do
    it "poo → sTTTssss sTTsTTTT sTTsTTTT"
      $ (split8 "sTTTsssssTTsTTTTsTTsTTTT" [])
      `shouldBe` ["sTTTssss", "sTTsTTTT", "sTTsTTTT"]
    it "sTTTssss sTTsTTTT sTTsTTTT → poo"
      $ (makeAsciiName "sTTTsssssTTsTTTTsTTsTTTT") `shouldBe` "poo"
    labelToHuman ""
    labelToHuman "a"
    labelToHuman "print-hello"
