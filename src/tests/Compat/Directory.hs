module Compat.Directory where
import System.Directory

-- Also defined by directory >= 1.2.5.0, released December 2015
listDirectory :: FilePath -> IO [FilePath]
listDirectory = fmap (filter (`notElem` [".", ".."])) . getDirectoryContents
