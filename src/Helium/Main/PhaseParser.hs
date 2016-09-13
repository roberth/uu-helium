{-| Module      :  PhaseParser
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.PhaseParser(phaseParser) where

import Helium.Main.CompileUtils
import Helium.Parser.LexerToken(Token)
import Helium.Parser.Parser (module_)
import Helium.Parser.ParseLibrary(runHParser)
import Text.ParserCombinators.Parsec.Error (ParseError)
import Helium.MonadCompile

phaseParser :: MonadCompile m =>
   String -> [Token] -> [Option] -> 
   m (Either [ParseError] Module)
phaseParser fullName tokens options = do
    enterNewPhase "Parsing"
    case runHParser module_ fullName tokens True of
        Left parseError ->
            return (Left [parseError])
        Right m ->
            return (Right m)
