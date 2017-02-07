module Main where
import Test.Tasty
import qualified Helium.Test.Examples
import qualified Helium.Test.Custom

main :: IO ()
main = do
  examples <- Helium.Test.Examples.determineTests
  custom <- Helium.Test.Custom.determineTests
  defaultMain $ testGroup "Tests" $ [examples, custom]
