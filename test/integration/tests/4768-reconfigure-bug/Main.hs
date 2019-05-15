import StackTest
import System.Directory (copyFile)

main :: IO ()
main = do
  stack ["clean", "--full"]
  copyFile "a/to-copy/A1.hs" "a/src/A.hs"
  stack ["build", "a"]
  stack ["build", "c"]

  copyFile "a/to-copy/A2.hs" "a/src/A.hs"
  stack ["build", "a"]
  stack ["build", "c"]
