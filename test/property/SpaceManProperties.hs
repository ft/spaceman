import Control.Monad (unless)
import System.Environment (getArgs)
import Test.QuickCheck
import Text.Printf (printf)

import PropertiesHumanLabel

tests =
  [("For printables, makeAsciiName is inverse of label", humanLabelInverse)]

defaultNumberOfTests :: Int
defaultNumberOfTests = 1000

defaultSize :: Int
defaultSize = 100

howManyTests :: [String] -> Int
howManyTests [s] = read s
howManyTests _ = defaultNumberOfTests

testArgs :: Int -> Args
testArgs n = stdArgs { maxSuccess = n,
                       maxSize = defaultSize }

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
  let n = howManyTests arg
    in do
    failn <- length . filter not <$> mapM (run n) tests
    unless (failn == 0) $ error $ show failn ++ " test(s) failed"
