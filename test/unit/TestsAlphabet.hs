module TestsAlphabet (testsAlphabet) where

import Test.Hspec

import SpaceMan.Alphabet

t :: Bool -> String -> String -> String
t is ab ch = ch ++ " is"
                ++ (if is then "" else " NOT")
                ++ " in the " ++ ab ++ " alphabet"

testsAlphabet = hspec $ do
  describe "Alphabet.Characters" $ do
    it "<space> is ' '"  $ space    `shouldBe` ' '
    it "<lf> is '\\n'"   $ linefeed `shouldBe` '\n'
    it "<tab> is '\\t'"  $ tabular  `shouldBe` '\t'
  describe "Alphabet.WhitespaceAlphabet" $ do
    it (t True "ws" "<space>") $ (fromWhitespaceAlphabet ' ')  `shouldBe` True
    it (t True "ws" "<lf>")    $ (fromWhitespaceAlphabet '\n') `shouldBe` True
    it (t True "ws" "<tab>")   $ (fromWhitespaceAlphabet '\t') `shouldBe` True
  describe "Alphabet.IntegerAlphabet" $ do
    it (t True  "int" "<space>") $ (fromIntegerAlphabet ' ')  `shouldBe` True
    it (t False "int" "<lf>")    $ (fromIntegerAlphabet '\n') `shouldBe` False
    it (t True  "int" "<tab>")   $ (fromIntegerAlphabet '\t') `shouldBe` True
