{-| Module      :  PhaseTypeInferencer
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.PhaseTypeInferencer (phaseTypeInferencer) where

import Helium.Main.CompileUtils
import Helium.StaticAnalysis.Messages.Warnings(Warning)
import Helium.StaticAnalysis.Inferencers.TypeInferencing(typeInferencing)
import Helium.ModuleSystem.DictionaryEnvironment (DictionaryEnvironment)
--import UHA_Syntax
import Helium.StaticAnalysis.Messages.TypeErrors
import Helium.StaticAnalysis.Messages.Information (showInformation')
import Helium.StaticAnalysis.Messages.HeliumMessages (sortAndShowMessages)
import System.FilePath.Posix
import Helium.MonadCompile

phaseTypeInferencer :: MonadCompile m =>
    String -> String -> Module -> ImportEnvironment -> ImportEnvironment -> [Option] -> 
    m (Either [TypeError] (DictionaryEnvironment, ImportEnvironment, TypeEnvironment, [Warning]))

phaseTypeInferencer basedir fullName module_ localEnv completeEnv options = do
    enterNewPhase "Type inferencing"

    -- 'W' and 'M' are predefined type inference algorithms
    let newOptions = (if AlgorithmW `elem` options
                        then filter (/= NoSpreading) . ([TreeWalkInorderTopLastPost, SolverGreedy]++) 
                        else id)
                   . (if AlgorithmM `elem` options
                        then filter (/= NoSpreading) . ([TreeWalkInorderTopFirstPre, SolverGreedy]++)  
                        else id)
                   $ options
                   
        (debugString, dictionaryEnv, toplevelTypes, typeErrors, warnings) =
           typeInferencing newOptions completeEnv module_

        -- add the top-level types (including the inferred types)
        finalEnv = addToTypeEnvironment toplevelTypes completeEnv

    when (DumpTypeDebug `elem` options) (logMessage debugString)

    -- display name information
    let i = showInformation' True options finalEnv
    when (not (null i)) $ logMessage $ sortAndShowMessages i

    case typeErrors of 
       
       _:_ ->
          do when (DumpInformationForAllModules `elem` options) $
                logMessage (show completeEnv)
             return (Left typeErrors)
          
       [] -> 
          do -- Dump information
             when (DumpInformationForAllModules `elem` options) $ 
                logMessage $ show finalEnv
             when (HFullQualification `elem` options) $
                writeFullQualificationFile (combinePathAndFile basedir (dropExtension $ takeFileName fullName) ++ ".fqn") 
                          (holmesShowImpEnv module_ finalEnv)
             when (  DumpInformationForThisModule `elem` options 
                  && DumpInformationForAllModules `notElem` options
                  ) 
                  $ logMessage $ show (addToTypeEnvironment toplevelTypes localEnv)
                  
             return (Right (dictionaryEnv, finalEnv, toplevelTypes, warnings))
