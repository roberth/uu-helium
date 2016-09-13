{-| Module      :  PhaseCodeGenerator
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.PhaseCodeGenerator(phaseCodeGenerator) where

import Lvm.Core.Expr(CoreModule)
import Helium.Main.CompileUtils
import Helium.CodeGeneration.CoreToLvm(coreToLvm)
import Helium.MonadCompile

phaseCodeGenerator :: MonadCompile m => String -> CoreModule -> m ()
phaseCodeGenerator fullName coreModule = do
    enterNewPhase "Code generation"

    let (path, baseName, _) = splitFilePath fullName
        fullNameNoExt = combinePathAndFile path baseName

    bytes <- coreToLvm coreModule
    writeLvmFile (fullNameNoExt ++ ".lvm") bytes
