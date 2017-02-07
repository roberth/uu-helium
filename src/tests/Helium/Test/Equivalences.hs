-- * Stuff humans would (hopefully) consider equivalent.
module Helium.Test.Equivalences
  ( eqMessages
  ) where
import           Data.Function      (on)
import           Helium.Test.String

-- | String equality modulo trailing spaces modulo empty lines
eqMessages :: [String] -> [String] -> Bool
eqMessages = (==) `on` (noEmptyLines . map trimr . normalizeLines)
  where normalizeLines = lines . unlines
        noEmptyLines = filter (not . null)
