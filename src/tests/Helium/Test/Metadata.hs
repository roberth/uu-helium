
-- * Reading metadata from test case source file comments
module Helium.Test.Metadata
  ( parseDict
  ) where

import           Control.Arrow      ((***))
import           Data.List
import           Helium.Test.String

parseDict :: String -> [(String, String)]
parseDict =
  let pfx = "--- "
      toKeyValue = (triml *** (triml1 . safeTail)) . span (/= ':')
      safeTail (_:tl) = tl
      safeTail []     = []
      mergeLines (l:('|':' ':m):ls) = mergeLines ((l ++ '\n' : m) : ls)
      mergeLines (n:ns)             = n : mergeLines ns
      mergeLines []                 = []
  in map toKeyValue . mergeLines . map (drop (length pfx)) . filter (isPrefixOf pfx) . lines

