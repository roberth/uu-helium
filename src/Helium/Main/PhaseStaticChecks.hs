{-| Module      :  PhaseStaticChecks
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.PhaseStaticChecks(phaseStaticChecks) where

import Helium.Main.CompileUtils
import Helium.StaticAnalysis.Messages.Warnings(Warning)
import qualified Helium.StaticAnalysis.StaticChecks.StaticChecks as SC
import Helium.Syntax.UHA_Syntax (Name)
import Top.Types (TpScheme)
import Helium.StaticAnalysis.Messages.StaticErrors
import Helium.StaticAnalysis.Messages.Information (showInformation')
import Helium.StaticAnalysis.Messages.HeliumMessages (showMessage)
import Helium.MonadCompile

phaseStaticChecks :: MonadCompile m =>
   String -> Module -> [ImportEnvironment] -> [Option] -> 
   m (Either [Error] (ImportEnvironment, [(Name,TpScheme)], [Warning]))
phaseStaticChecks fullName module_ importEnvs options = do
    enterNewPhase "Static checking"

    let (_, baseName, _) = splitFilePath fullName

        res = SC.wrap_Module (SC.sem_Module module_) SC.Inh_Module {
                 SC.baseName_Inh_Module = baseName,
                 SC.importEnvironments_Inh_Module = importEnvs,
                 SC.options_Inh_Module = options }

    case SC.errors_Syn_Module res of
    
       _:_ ->
          do when (DumpInformationForAllModules `elem` options) $
                logMessage $ show $ (foldr combineImportEnvironments emptyEnvironment importEnvs)
             
             -- display name information
             let combinedEnv = foldr combineImportEnvironments emptyEnvironment importEnvs
             let info = showInformation' False options combinedEnv
             mapM_ (logMessage . showMessage) info

             return (Left $ SC.errors_Syn_Module res)
         
       [] -> 
          return (Right (SC.collectEnvironment_Syn_Module res, SC.typeSignatures_Syn_Module res, SC.warnings_Syn_Module res))
