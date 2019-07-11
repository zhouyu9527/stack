import StackTest
import qualified Data.ByteString as B

main :: IO ()
main = do
  stack ["build", "--dry-run"]
  B.readFile "umlaut.cabal" >>= print
  stack ["build"]
