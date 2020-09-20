module PropertiesHumanLabel (humanLabelInverse) where

import Data.Char
import Test.QuickCheck

import SpaceMan.Generate as G
import SpaceMan.Transform as T

humanLabelChar :: Gen Char
humanLabelChar = oneof $ map return [ c | c <- map chr [0..127], isPrint c ]

newtype HumanLabel = HumanLabel String deriving (Show)

instance Arbitrary HumanLabel where
  arbitrary = HumanLabel <$> listOf humanLabelChar

humanLabelInverse :: HumanLabel -> Bool
humanLabelInverse (HumanLabel s) = (T.makeAsciiName . G.label) s == s
