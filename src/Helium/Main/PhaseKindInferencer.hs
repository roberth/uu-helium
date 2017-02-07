{-| Module      :  PhaseKindInferencer
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.PhaseKindInferencer
  ( phaseKindInferencer
  , KindEnvironment
  ) where

import Helium.Main.CompileUtils
import Helium.StaticAnalysis.Inferencers.KindInferencing as KI
import Top.Types
import Helium.StaticAnalysis.Messages.KindErrors
import Helium.MonadCompile
import qualified Data.Map as M

phaseKindInferencer :: MonadCompile m => ImportEnvironment -> Module -> m (Either [KindError] KindEnvironment)
phaseKindInferencer importEnvironment module_ =
  do enterNewPhase "Kind inferencing"
     options <- compilationOptions
     let (debugString, kindEnv, kindErrors) = KI.inferKinds importEnvironment module_ options
     whenEnabled_ DumpTypeDebug $ 
         do logMessage $ debugString
            logMessage "Kinds: ########################################"
            logMessage . unlines . map (\(n,ks) -> show n++" :: "++showKindScheme ks) $ M.assocs $ kindEnv
            logMessage "End kinds"

     return $ case kindErrors of
         _:_ -> Left kindErrors
         [] -> Right kindEnv
