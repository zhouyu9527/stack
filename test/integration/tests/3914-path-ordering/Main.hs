import StackTest
import Control.Monad (unless)
import System.Directory (copyFile)

main :: IO ()
main = do
  copyFile "Main1.hs" "Main.hs"
  stack ["install"]
  stackCheckStdout ["exec", "foo"] $ \str -> unless (str == "Main1\n") $ error $ "Should be Main1, got: " ++ show str

  copyFile "Main2.hs" "Main.hs"
  stack ["install"]
  stackCheckStdout ["exec", "foo"] $ \str -> unless (str == "Main2\n") $ error $ "Should be Main2, got: " ++ show str
