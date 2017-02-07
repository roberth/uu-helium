{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
module Helium.Test.Custom
  ( determineTests
  ) where
import           Prelude hiding(interact)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import           Compat.Directory
import           Data.IORef
import           Data.List(isSuffixOf,isInfixOf,sort)
import qualified Data.ByteString as B
import qualified Helium.Compilation
import           Helium.Main.CompileUtils (Option, makeCoreLib)
import           Helium.Main.Args         (argsToOptions)
import           Helium.Test.FreeTest
import           Lvm.Common.Id         (stringFromId, newNameSupply)
import           Lvm.Common.Byte       (listFromBytes)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Helium.Test.CompileAction
import           System.FilePath((</>))
import           Helium.Test.Metadata
import           Helium.Test.Equivalences

myOptions :: [Option]
myOptions = argsToOptions ["--overloading", "--kind-inferencing"]

handleTrivialEvents ::
  (Member IO r, Member (Writer [String]) r) =>
  Eff (CompileAction ': r) a -> Eff (CompileAction ': r) a
handleTrivialEvents = handlePart $ \case
                       LogMessage x -> do
                         tell ["Message: " ++ x]
                         now ()
                       SetDebugFileName _ -> now ()
                       SetDebugDoneModules _ -> now ()
                       CompilationOptions -> now myOptions
                       EnterNewPhase phase -> do
                         tell ["Phase:   " ++ phase]
                         now ()
                       SubmitLog _ _ _ -> now ()
                       CreateNameSupply -> send newNameSupply >>= now
                       ReadTypingStrategiesFile _ -> now mempty
                       _other -> defer

-- | Augments the exception with the log when an exception occurs.
withTestLog :: (IORef [String] -> IO b) -> IO b
withTestLog f = do
  msgLog <- newIORef mempty
  f msgLog `catch` \e -> do
        logLines <- readIORef msgLog
        let prependLog = if null logLines then id
                         else (\x -> "Exception occurred during test. Here's the log first:\n"
                                ++ unlines logLines
                                ++ "\nException: \n"
                                ++ x)
        error $ prependLog $ show (e :: SomeException)

runTest :: Eff '[CompileAction, Writer [String], TestControl, IO] a             -- ^ Test subject
        -> Eff '[Interact CompileAction '[Writer [String], TestControl, IO] a, 
                  Writer [String], TestControl, IO] b                           -- ^ Test script
        -> IO ()
runTest subject expectations = withTestLog $ \testLog -> do
    runIO $ runTestControl_ $ void $ runWriterIO testLog $
      runInteractions (handleTrivialEvents subject) expectations

runTest' :: Eff '[CompileAction, Writer [String], TestControl, IO] a            -- ^ Test subject
        -> Eff '[Interact CompileAction '[Writer [String], TestControl, IO] a,
                  Writer [String], TestControl, IO] b                           -- ^ Test script
        -> IO b
runTest' subject expectations = withTestLog $ \testLog -> do
    runIO $ runTestControl' $ runWriterIO testLog $
      runInteractions (handleTrivialEvents subject) expectations

testcase :: [(String, B.ByteString)] -> IO TestTree
testcase modules = do
  return $ testCase "The test case" $ do
    let subject = Helium.Compilation.compile "no-basedir" "Test.hs" []
    runTest (handleImports modules subject) $ do
      stub $ \case ReadSourceFile "Test.hs" -> return "module Test where\nimport TestClasses\nhoi = ()"
                   x -> unexpected x
      stub $ \case PrintWarningMessage m | "Warning: Missing type signature: hoi" `isInfixOf` m -> return $ ()
                   x -> unexpected x
      stub $ \case WriteLvmFile _ _ -> fulfilled
                   x -> unexpected x

determineTests :: IO TestTree
determineTests = do
  deps <- buildDependencies
  tc <- testcase deps
  tcd <- testCasesFromDirectory deps "typeClassesKindInferencing"
  return $ testGroup "Custom" (tc:tcd)

handleImports :: [(String, B.ByteString)] -> Eff (CompileAction ': r) a -> Eff (CompileAction ': r) a
handleImports modules = handlePart $
  \case FindLvmFile x | Just b <- lookup (stringFromId x) modules
                        -> now $ B.unpack b
        _ -> defer

testCasesFromDirectory :: [(String, B.ByteString)] -- ^ Pre-built LVM modules
                       -> FilePath                 -- ^ Directory to load all .hs testcases from
                       -> IO [TestTree]
testCasesFromDirectory deps dirName = do
  let dirPath = ("." </> "test" </> dirName)
  t <- listDirectory dirPath
  concat <$> (forM (sort t) $ \fileName ->
    case fileName of
      hsFile | ".hs" `isSuffixOf` hsFile ->
               return <$> mkTestCase deps dirPath hsFile
      _ -> return [])

mkTestCase :: [(String, B.ByteString)] -- ^ Pre-built LVM modules
  -> FilePath                          -- ^ Directory
  -> FilePath                          -- ^ File name relative to directory
  -> IO TestTree
mkTestCase deps dirPath hsFile = do
  return $ testCase ("Example " ++ hsFile) $ do
    source <- readFile $ dirPath </> hsFile
    let dict = parseDict source
        dictEntries key = [ v | (k, v) <- dict, k == key ]
    let opts = argsToOptions (dictEntries "option")

    let subject = Helium.Compilation.compile "no-basedir" hsFile []

    runTest (handlePart (\case CompilationOptions -> now opts
                               _ -> defer) subject) $ do
      
      stub $ \case ReadSourceFile x | x == hsFile -> return source
      stubLvmFiles deps
      
      if null $ dictEntries "ok"
        then do
          -- expect error
          errors <- collect $ \case PrintErrorMessage m -> now ((), [m])
                                    PrintWarningMessage _ -> now ((), [])
                                    x -> defer
          stub $ \case Abort -> do
                         let expected = dictEntries "error"
                             actual = concat errors
                         if actual `eqMessages` expected
                           then fulfilled
                           else error $
                                "Errors did not match. Was:\n" ++ unlines actual ++ "\n" ++
                                "Expected:\n" ++ unlines expected

                         fulfilled
                       x -> expecting "failed compilation" x
          

        else do
          stub $ \case WriteLvmFile _ _ -> return ()
                       x -> expecting "writeLvmFile (succeeding compilation)" x
          -- expect success
          void $ result

-- | Provides the passed in LVM files to the compiler, when testing.
stubLvmFiles :: [(String, B.ByteString)]
  -> Eff (Interact CompileAction r v ': r) ()
stubLvmFiles modules =
  stubs $ \case FindLvmFile x -> now $ B.unpack $
                    maybe (error $ ".lvm not found: " ++ show x) id $
                    lookup (stringFromId x) modules
                x -> defer

buildDependencies :: IO [(String, B.ByteString)]
buildDependencies = do
  coreModules <- buildCoreLvmFiles
  
  let subject = Helium.Compilation.compile "no-basedir" "lib/Prelude.hs" []

  prelude <- runTest' subject $ do
    
    stub $ \case ReadSourceFile "lib/Prelude.hs" ->
                   send $ readFile "lib/Prelude.hs"
                 x -> unexpected x

    stubs $ \case FindLvmFile x -> now $ B.unpack $
                    maybe (error $ ".lvm not found: " ++ show x) id $
                    lookup (stringFromId x) coreModules
                  x -> defer

    preludeLvm <-
      expect $ \case WriteLvmFile path b
                       | "Prelude.lvm" `isSuffixOf` path ->
                         spy b
                     x -> unexpected x

    return ("Prelude", B.pack (listFromBytes preludeLvm))

  let preludeModules = prelude : coreModules

  let subject2 = Helium.Compilation.compile "no-basedir" "lib/TestClasses.hs" []

  testClasses <- runTest' subject2 $ do
    
    stub $ \case ReadSourceFile "lib/TestClasses.hs" ->
                   send $ readFile "test/lib/TestClasses.hs"
                 x -> unexpected x

    stubs $ \case FindLvmFile x -> now $ B.unpack $
                    maybe (error $ ".lvm not found: " ++ show x) id $
                    lookup (stringFromId x) preludeModules
                  x -> defer

    lvm <-
      expect $ \case WriteLvmFile path b
                       | "lib//TestClasses.lvm" `isSuffixOf` path ->
                         spy b
                     x -> unexpected x

    return ("TestClasses", B.pack (listFromBytes lvm))

  return (testClasses : preludeModules)

-- This implementation caches the .lvm's in ./lib
buildCoreLvmFiles :: IO [(String, B.ByteString)]
buildCoreLvmFiles = do
  -- Order matters
  let coreLibs = ["LvmLang", "LvmIO", "LvmException", "HeliumLang", "PreludePrim"]

  forM coreLibs $ \moduleName -> do
    makeCoreLib "lib" moduleName
    (\bs -> (moduleName, bs)) <$> B.readFile ("lib/" ++ moduleName ++ ".lvm")
