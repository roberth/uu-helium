module Main where
import Test.DocTest
import System.Environment

main = do
  doctest ["-isrc"
          , "-idist/build/autogen"
          , "src/Helium/Parser/Parser.hs"
          , "src/Helium/ModuleSystem/KindParser.hs"
          ]
