{-| Module      :  TS_Compile
    License     :  GPL

    Maintainer  :  bastiaan@cs.uu.nl
    Stability   :  experimental
    Portability :  portable

	Compile a .type file.
	
	(directives based on "Scripting the Type Inference Process", ICFP 2003)
-}

module Helium.StaticAnalysis.Directives.TS_Compile where

import Helium.StaticAnalysis.Directives.TS_CoreSyntax
import Helium.ModuleSystem.ImportEnvironment
import Helium.StaticAnalysis.Directives.TS_ToCore      (typingStrategyToCore)
import Helium.StaticAnalysis.Directives.TS_Parser      (parseTypingStrategies)
import Helium.Parser.Lexer          (strategiesLexer)
import Helium.StaticAnalysis.Directives.TS_Analyse     (analyseTypingStrategies)
import Control.Monad          (unless, when)
import qualified Helium.Main.Args as Args
import Helium.Parser.ParseMessage ()
import Helium.CodeGeneration.CoreUtils
import Lvm.Core.Expr
import Helium.MonadCompile
import Helium.StaticAnalysis.Messages.HeliumMessages(showMessage)
import Helium.StaticAnalysis.Messages.Messages(HasMessage,sortMessages)
import Control.Monad(void)

readTypingStrategiesFromFile :: MonadCompile m => String -> ImportEnvironment ->
    m (Core_TypingStrategies, [CoreDecl])
readTypingStrategiesFromFile filename importEnvironment = do
  typingStategies <- readTypingStrategiesFile filename
  case typingStategies of
    Nothing -> return mempty
    Just fileContent -> do
      options <- compilationOptions
      let rightOrAbort (Left err) = do
            printErrorMessage "Parse error in typing strategy:"
            printSortedErrors [err]
            abort
          rightOrAbort (Right x) = return x
      (tokens, _) <- rightOrAbort (strategiesLexer options filename fileContent)
      strategies <- rightOrAbort (parseTypingStrategies (operatorTable importEnvironment) filename tokens)

      let (errors, warnings) = analyseTypingStrategies strategies importEnvironment

      unless (null errors) $ do
        printSortedErrors errors
        abort

      unless (null warnings) $ do
        void $ whenDisabled Args.NoWarnings $ do
          printWarningMessage "Warnings in typing strategies:"

      let number = length strategies
      void $ whenEnabled Args.Verbose $ do
        when (number > 0) $
          logMessage ("   (" ++
                   (if number == 1
                     then "1 strategy is included)"
                     else show number ++ " strategies are included)"))

      let coreTypingStrategies = map (typingStrategyToCore importEnvironment) strategies
      void $ whenEnabled Args.DumpTypeDebug $ do
        logMessage "Core typing strategies:"
        mapM_ (logMessage . show) coreTypingStrategies

      return ( coreTypingStrategies, [ customStrategy (show coreTypingStrategies) ] )

printSortedWarnings, printSortedErrors :: (MonadCompile m, HasMessage msg) => [msg] -> m ()
printSortedWarnings  = mapM_ (printWarningMessage . showMessage) . sortMessages
printSortedErrors    = mapM_ (printErrorMessage   . showMessage) . sortMessages

