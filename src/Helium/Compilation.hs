
{-| Module      :  Compilation
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compilation where

import           Helium.Main.Args                              (overloadingFromOptions)
import           Helium.Main.CompileUtils
import           Helium.Main.PhaseCodeGenerator
import           Helium.Main.PhaseDesugarer
import           Helium.Main.PhaseImport
import           Helium.Main.PhaseKindInferencer
import           Helium.Main.PhaseLexer
import           Helium.Main.PhaseParser
import           Helium.Main.PhaseResolveOperators
import           Helium.Main.PhaseStaticChecks
import           Helium.Main.PhaseTypeInferencer
import           Helium.Main.PhaseTypingStrategies
import           Helium.MonadCompile
import           Helium.Parser.Lexer                           (checkTokenStreamForClassOrInstance)
import           Helium.StaticAnalysis.Messages.HeliumMessages (showMessage)
import           Helium.StaticAnalysis.Messages.Messages       (HasMessage,
                                                                sortMessages)
import           Helium.StaticAnalysis.Messages.StaticErrors   (errorsLogCode)

compile :: MonadCompile m =>
           String -> [Char] -> [String] -> m ()
compile basedir fullName doneModules =
    do
        logMessage ("Compiling " ++ fullName)
        setDebugFileName fullName
        setDebugDoneModules doneModules

        options <- compilationOptions
        let compileOptions = (options, fullName, doneModules)

        contents <- readSourceFile fullName

        -- Phase 1: Lexing
        (lexerWarnings, tokens) <-
            doPhaseWithExit 20 (const "L") compileOptions $
                phaseLexer fullName contents options

        whenDisabled_ NoWarnings $
            printSortedWarnings lexerWarnings

        -- If the token stream contains the words class or instance
        -- and overloading is off, then print error message and bail out:
        unless (overloadingFromOptions options) $ do
           let classInstanceMessages = checkTokenStreamForClassOrInstance tokens
           unless (null classInstanceMessages) $ do
                printSortedErrors classInstanceMessages
                abort

        -- Phase 2: Parsing
        parsedModule <-
            doPhaseWithExit 20 (const "P") compileOptions $
               phaseParser fullName tokens options

        -- Phase 3: Importing
        (indirectionDecls, importEnvs) <-
            phaseImport fullName parsedModule

        -- Phase 4: Resolving operators
        resolvedModule <-
            doPhaseWithExit 20 (const "R") compileOptions $
               phaseResolveOperators parsedModule importEnvs options

        whenEnabled_ StopAfterParser abort

        -- Phase 5: Static checking
        (localEnv, typeSignatures, staticWarnings) <- do
            logMessage $ "Import envs: " ++ (show importEnvs)
            doPhaseWithExit 20 (("S"++) . errorsLogCode) compileOptions $
               phaseStaticChecks fullName resolvedModule importEnvs options

        whenDisabled_ NoWarnings $
            printSortedWarnings staticWarnings

        whenEnabled_ StopAfterStaticAnalysis abort

        -- Phase 6: Kind inferencing (by default turned off)
        let combinedEnv = foldr combineImportEnvironments localEnv importEnvs
        kindEnv <- whenEnabled KindInferencing $
           doPhaseWithExit maximumNumberOfKindErrors (const "K") compileOptions $
              phaseKindInferencer combinedEnv resolvedModule

        -- Phase 7: Type Inference Directives
        (beforeTypeInferEnv, typingStrategiesDecls) <-
            phaseTypingStrategies fullName combinedEnv typeSignatures options

        -- Phase 8: Type inferencing
        (dictionaryEnv, afterTypeInferEnv, toplevelTypes, typeWarnings) <-
            doPhaseWithExit maximumNumberOfTypeErrors (const "T") compileOptions $
               phaseTypeInferencer basedir fullName resolvedModule {-doneModules-} localEnv beforeTypeInferEnv options

        whenDisabled_ NoWarnings $
           printSortedWarnings typeWarnings

        whenEnabled_ StopAfterTypeInferencing abort

        -- Phase 9: Desugaring
        coreModule <-
            phaseDesugarer dictionaryEnv
                           fullName resolvedModule
                           (typingStrategiesDecls ++ indirectionDecls)
                           (nothingToEmpty kindEnv)
                           afterTypeInferEnv
                           toplevelTypes

        whenEnabled_ StopAfterDesugar abort

        -- Phase 10: Code generation
        phaseCodeGenerator fullName coreModule

        submitLog "C" fullName doneModules

        let number = length staticWarnings + length typeWarnings + length lexerWarnings

        logMessage $ "Compilation successful" ++
                        if number == 0 || (NoWarnings `elem` options)
                          then ""
                          else " with " ++ show number ++ " warning" ++ if number == 1 then "" else "s"

printSortedWarnings, printSortedErrors :: (MonadCompile m, HasMessage msg) => [msg] -> m ()
printSortedWarnings  = mapM_ (printWarningMessage . showMessage) . sortMessages
printSortedErrors    = mapM_ (printErrorMessage   . showMessage) . sortMessages

maximumNumberOfTypeErrors :: Int
maximumNumberOfTypeErrors = 3

maximumNumberOfKindErrors :: Int
maximumNumberOfKindErrors = 1

nothingToEmpty :: Monoid a => Maybe a -> a
nothingToEmpty Nothing = mempty
nothingToEmpty (Just x) = x
