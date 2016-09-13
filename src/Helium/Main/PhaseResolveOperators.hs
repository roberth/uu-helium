{-| Module      :  PhaseResolveOperators
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.PhaseResolveOperators(phaseResolveOperators) where

import Helium.Main.CompileUtils
import Helium.Parser.ResolveOperators(resolveOperators, operatorsFromModule, ResolveError)
import qualified Helium.Syntax.UHA_Pretty as PP(sem_Module,wrap_Module,Inh_Module(..),text_Syn_Module)
import qualified Data.Map as M
import Helium.MonadCompile

phaseResolveOperators :: MonadCompile m =>
   Module -> [ImportEnvironment] -> [Option] -> 
   m (Either [ResolveError] Module)

phaseResolveOperators moduleBeforeResolve importEnvs options = do
    enterNewPhase "Resolving operators"

    let importOperatorTable = 
            M.unions (operatorsFromModule moduleBeforeResolve : map operatorTable importEnvs)
                          
        (module_, resolveErrors) = 
                  resolveOperators importOperatorTable moduleBeforeResolve

    case resolveErrors of
       
       _:_ ->
          return (Left resolveErrors)
          
       [] ->
          do when (DumpUHA `elem` options) $
                logMessage $ show $ PP.text_Syn_Module $ PP.wrap_Module (PP.sem_Module module_) PP.Inh_Module
    
             return (Right module_)

    

