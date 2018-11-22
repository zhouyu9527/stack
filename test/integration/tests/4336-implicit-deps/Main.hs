import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
  stackCheckStderr ["build", "acme-missiles"] $ \err ->
    unless ("packages were added implicitly" `isInfixOf` err) $
      error $ "Explected warning about implicitly added packages but got " ++ show err
