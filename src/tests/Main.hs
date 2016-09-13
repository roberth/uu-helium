module Main where
import Test.Tasty
import qualified Helium.Test.Examples

main :: IO ()
main = do
  examples <- Helium.Test.Examples.determineTests
  defaultMain examples
