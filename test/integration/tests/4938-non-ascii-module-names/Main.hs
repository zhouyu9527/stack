import StackTest

main :: IO ()
main = do
  stack ["build", "--dry-run"]
  readFile "umlaut.cabal" >>= putStrLn
  stack ["build"]
