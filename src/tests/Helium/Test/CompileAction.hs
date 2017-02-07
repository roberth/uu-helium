{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Helium.Test.CompileAction where
import Helium.MonadCompile
import Helium.Test.FreeTest
import Lvm.Common.Id(Id, NameSupply)
import Lvm.Common.Byte(Byte,Bytes)
import Helium.Main.CompileUtils (Option)

data CompileAction a where
  CompilationOptions :: CompileAction [Option]
  LogMessage :: String -> CompileAction ()
  PrintWarningMessage :: String -> CompileAction ()
  PrintErrorMessage :: String -> CompileAction ()
  Abort :: CompileAction a
  AbortPositive :: CompileAction a
  SetDebugFileName :: String -> CompileAction ()
  SetDebugDoneModules :: [String] -> CompileAction ()
  ReadSourceFile :: String -> CompileAction String
  ReadTypingStrategiesFile :: String -> CompileAction (Maybe String)
  EnterNewPhase :: String -> CompileAction ()
  WriteCoreFile :: FilePath -> String -> CompileAction ()
  WriteFullQualificationFile :: FilePath -> String -> CompileAction ()
  WriteLvmFile :: FilePath -> Bytes -> CompileAction ()
  FindLvmFile :: Id -> CompileAction [Byte]
  CreateNameSupply :: CompileAction NameSupply
  SubmitLog :: String -> String -> [String] -> CompileAction ()

instance MonadCompile (Eff (CompileAction ': more)) where
  compilationOptions = send CompilationOptions
  logMessage s = send $ LogMessage s
  printWarningMessage s = send $ PrintWarningMessage s
  printErrorMessage s = send $ PrintErrorMessage s
  abort = send Abort
  abortPositive = send AbortPositive
  setDebugFileName s = send $ SetDebugFileName s
  setDebugDoneModules ss = send $ SetDebugDoneModules ss
  readSourceFile s = send $ ReadSourceFile s
  readTypingStrategiesFile s = send $ ReadTypingStrategiesFile s
  enterNewPhase s = send $ EnterNewPhase s
  writeCoreFile p s = send $ WriteCoreFile p s
  writeFullQualificationFile p s = send $ WriteFullQualificationFile p s
  writeLvmFile p bs = send $ WriteLvmFile p bs
  findLvmFile m = send $ FindLvmFile m
  createNameSupply = send CreateNameSupply
  submitLog a b c = send $ SubmitLog a b c

instance Show1Q CompileAction where
  show1q CompilationOptions = "compilationOptions"
  show1q (LogMessage s) = "logMessage " ++ s
  show1q (PrintWarningMessage s) = "printWarningMessage " ++ show s
  show1q (PrintErrorMessage s) = "printErrorMessage " ++ show s
  show1q Abort = "abort"
  show1q AbortPositive = "abortPositive"
  show1q (SetDebugFileName s) = "setDebugFileName " ++ show s
  show1q (SetDebugDoneModules s) = "setDebugDoneModules " ++ show s
  show1q (ReadSourceFile s) = "readSourceFile " ++ show s
  show1q (ReadTypingStrategiesFile s) = "readTypingStrategiesFile " ++ show s
  show1q (EnterNewPhase s) = "enterNewPhase " ++ show s
  show1q (WriteCoreFile fp s) = "writeCoreFile " ++ show fp ++ " " ++ show s
  show1q (WriteFullQualificationFile fp s) = "writeFullQualificationFile " ++ show fp ++ " " ++ show s
  show1q (WriteLvmFile fp _bs) = "writeLvmFile " ++ show fp
  show1q (FindLvmFile ident) = "findLvmFile " ++ show ident
  show1q CreateNameSupply = "createNameSupply"
  show1q (SubmitLog _ _ _) = "submitLog"
