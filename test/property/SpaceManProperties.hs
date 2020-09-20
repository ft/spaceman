import Control.Monad
import Data.Char
import System.Environment
import Test.QuickCheck
import Text.Printf

import SpaceMan.Generate as G
import SpaceMan.Transform as T

humanLabelChar :: Gen Char
humanLabelChar = oneof $ map return [ c | c <- map chr [0..127], isPrint c ]

newtype HumanLabel = HumanLabel String deriving (Show)

instance Arbitrary HumanLabel where
  arbitrary = HumanLabel <$> listOf humanLabelChar

humanLabelInverse :: HumanLabel -> Bool
humanLabelInverse (HumanLabel s) = (T.makeAsciiName . G.label) s == s

tests =
  [("For printables, makeAsciiName is inverse of label", humanLabelInverse)]

defaultNumberOfTests :: [String] -> Int
defaultNumberOfTests [s] = read s
defaultNumberOfTests _ = 1000

testArgs :: Int -> Args
testArgs n = stdArgs { maxSuccess = n,
                       maxSize = 100 }

qc :: Testable p => Args -> p -> IO Bool
qc args test = do
  c <- quickCheckWithResult args test
  case c of Success {} -> return True
            _          -> return False

run :: Testable p => Int -> (String, p) -> IO Bool
run n (title, prop) = printf "%-50s: " title >> qc (testArgs n) prop

main :: IO ()
main = do
  arg <- getArgs
  let n = defaultNumberOfTests arg
    in do
    failn <- length . filter not <$> mapM (run n) tests
    unless (failn == 0) $ error $ show failn ++ " test(s) failed"
