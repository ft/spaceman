import Control.Monad (unless)
import System.Environment (getArgs)
import Test.QuickCheck
import Text.Printf (printf)

import PropertiesHumanLabel
import PropertiesAlphabet

tests :: [(String, Property)]
tests =
  [("For printables, makeAsciiName is inverse of label", property humanLabelInverse),
   ("fromIntegerAlphabet filters correctly",             property nonIntegerCharacters),
   ("fromWhitespaceAlphabet filters Comments",           property nonWhitespaceCharacters)]

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
run n (title, prop) = printf format title >> qc (testArgs n) prop
  where titleColumWidth = 50
        format = "%-" ++ show titleColumWidth ++ "s: "

main :: IO ()
main = do
  arg <- getArgs
  let n = howManyTests arg
    in do
    failn <- length . filter not <$> mapM (run n) tests
    unless (failn == 0) $ error $ show failn ++ " test(s) failed"
