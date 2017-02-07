-- * Assorted string operations
module Helium.Test.String where
import           Data.Char
import           Data.List

triml, trimr, triml1 :: String -> String
triml = dropWhile isSpace
trimr = dropWhileEnd isSpace
triml1 (s:x) | isSpace s = x
triml1 x     = x
