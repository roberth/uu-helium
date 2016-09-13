{-| Module      :  Compile
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.Compile where

import Helium.Main.CompileUtils
import qualified Helium.Compilation as Compilation
import Helium.Main.CompileIO

compile :: String -> String -> [Option] -> [String] -> [String] -> IO ()
compile basedir fullName options lvmPath doneModules =
  runCompileIO (Compilation.compile basedir fullName doneModules) (CompileEnv options lvmPath)
