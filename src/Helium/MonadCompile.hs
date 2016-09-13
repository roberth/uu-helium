{-| Module      :  MonadCompile
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.MonadCompile
  ( MonadCompile(..)
  , isEnabled, whenEnabled, whenEnabled_, whenDisabled, whenDisabled_
  , when'
  ) where


import Control.Monad.Reader
import Helium.Main.CompileUtils
import Helium.StaticAnalysis.Messages.Messages(HasMessage)
import qualified Lvm.Core.Expr as Core
import Lvm.Common.Id         (Id, NameSupply)
import Lvm.Common.Byte       (Byte,Bytes)

class Monad m => MonadCompile m where

  compilationOptions :: m [Option]

  logMessage :: String -> m ()

  printWarningMessage :: String -> m ()
  printErrorMessage :: String -> m ()

  abort :: m a
  abortPositive :: m a

  -- These are made available using a (somewhat) unsafe IORef
  -- to be used for the internal error bug report
  setDebugFileName :: String -> m ()
  setDebugDoneModules :: [String] -> m ()

  readSourceFile :: String -> m String
  readTypingStrategiesFile :: String -> m (Maybe String)

  doPhaseWithExit :: HasMessage err => Int -> ([err] -> String) -> CompileOptions -> m (Either [err] a) -> m a

  enterNewPhase :: String -> m ()
  enterNewPhase = enterNewPhase'

  writeCoreFile :: FilePath -> String -> m ()
  writeFullQualificationFile :: FilePath -> String -> m ()
  writeLvmFile :: FilePath -> Bytes -> m ()
  findLvmFile :: Id -> m [Byte]

  createNameSupply :: m NameSupply

  -- logCode fullName modules
  submitLog :: String -> String -> [String] -> m ()


isEnabled :: MonadCompile m => Option -> m Bool
isEnabled s = do
  opts <- compilationOptions
  return (s `elem` opts)

when' :: Monad m => Bool -> m a -> m (Maybe a)
when' b a = if b
  then do v <- a
          return (Just v)
  else return Nothing

whenEnabled :: MonadCompile m => Option -> m a -> m (Maybe a)
whenEnabled s a = do
  enabled <- isEnabled s
  when' enabled a

whenEnabled_ :: MonadCompile m => Option -> m a -> m ()
whenEnabled_ o m = void (whenEnabled o m)

whenDisabled :: MonadCompile m => Option -> m a -> m (Maybe a)
whenDisabled s a = do
  enabled <- isEnabled s
  when' (not enabled) a

whenDisabled_ :: MonadCompile m => Option -> m a -> m ()
whenDisabled_ o m = void (whenDisabled o m)

enterNewPhase' :: MonadCompile m => String -> m ()
enterNewPhase' phase  =
   whenEnabled Verbose (logMessage (phase ++ "...")) >> return ()
