import Test.Hspec

import SpaceMan.Alphabet

isInWS :: String -> String
isInWS ch = ch ++ "is in the alphabet"

isInInt :: Bool -> String -> String
isInInt isit ch = ch ++ "is "
                     ++ if isit then "" else " NOT"
                     ++ " in the alphabet"

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
