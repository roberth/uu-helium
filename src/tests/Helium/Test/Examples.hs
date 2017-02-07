{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Helium.Test.Examples( determineTests
                           ) where
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.RWS
import qualified Data.ByteString          as B
import           Data.Foldable
import           Data.List
import qualified Helium.Compilation
import           Helium.Main.Args         (argsToOptions)
import           Helium.Main.CompileUtils (makeCoreLib)
import           Lvm.Common.Byte
import           System.FilePath          ((</>))

import           Compat.Directory

import           Helium.Test.Equivalences
import           Helium.Test.Metadata
import           Helium.Test.TestIO

-- This implementation caches the .lvm's in ./lib
buildCoreLvmFiles :: IO [(String, B.ByteString)]
buildCoreLvmFiles = do
  -- Order matters
  let coreLibs = ["LvmLang", "LvmIO", "LvmException", "HeliumLang", "PreludePrim"]

  forM coreLibs $ \moduleName -> do
    makeCoreLib "lib" moduleName
    (\bs -> (moduleName, bs)) <$> B.readFile ("lib/" ++ moduleName ++ ".lvm")

determineTests :: IO TestTree
determineTests = do

  coreLvmFiles <- buildCoreLvmFiles

  prelude <- do
    let basedir = "no-basedir"
        fullName = "Prelude.hs"
        doneModules = []
    contents <- readFile ("lib/" </> fullName)

    let env = mempty { sources = [(fullName, contents)]
                     , options = argsToOptions ["--overloading", "--kind-inferencing"]
                     , lvmFiles = coreLvmFiles
                     }
    (unitOrErr, _state, out) <- runRWST (runExceptT $ (fromTestIO (Helium.Compilation.compile basedir fullName doneModules))) env ()
    events <- case unitOrErr of
                Left Abort -> do
                  error ("Unexpected compilation error:\n" ++ show out)
                Left AbortPositive -> do
                  error ("Unexpected non-error interruption of compilation:\n" ++ show out)
                Left (TestFailure msg) -> do
                  error ("Test prerequisite failed because: " ++ msg ++
                         "\n\nOutput of test fixture: " ++ show out ++
                         "\n")
                Right () -> return $ toList $ outputEvents out
    let needle (LVMWritten "Prelude.lvm" x) = Just x
        needle _                            = Nothing
        collect f = Data.Foldable.foldr (\item more -> f item <|> more) Nothing
    case collect needle events of
      Nothing -> error ("Prelude.lvm not written!\n" ++ show out)
      Just x  -> return (B.pack (listFromBytes x))

  let baseEnv = mempty
                { lvmFiles = [("Prelude", prelude)] `mappend` coreLvmFiles
                }

  let testCasesFromDirectory dirName =
        do
          let dirPath = ("." </> "test" </> dirName)
          t <- listDirectory dirPath

          forM (sort t) $ \fileName ->
            case fileName of
              hsFile | ".hs" `isSuffixOf` hsFile -> fmap (:[]) $ mkTestCase baseEnv dirPath hsFile
              _ -> return []

  let testGroupFromDirectory dirName =
         (testGroup dirName . concat) <$> testCasesFromDirectory dirName

  --groups <- traverse testGroupFromDirectory ["typeClassesInstancesTyping/icebox"]
  groups <- traverse testGroupFromDirectory
            [ "typeClassesTypeInferencing"
            --, "typeClassesKindInferencing"
            ]
  return $ testGroup "Examples" groups

mkTestCase :: Environment -> FilePath -> String -> IO TestTree
mkTestCase baseEnv dirPath hsFile = do
  return $ testCase ("Test case " ++ hsFile) $ do
    source <- readFile $ dirPath </> hsFile
    let dict = parseDict source
        dictEntries key = [ v | (k, v) <- dict, k == key ]
    let env = baseEnv
                { sources = [(hsFile, source)]
                , options = argsToOptions (dictEntries "option")
                }
        basedir = "no-basedir"
        doneModules = []
    (unitOrErr, _state, out) <- runRWST (runExceptT $ (fromTestIO (Helium.Compilation.compile basedir hsFile doneModules))) env ()

    let events = toList (outputEvents out)

        assertError = do
          case unitOrErr of
                Left Abort -> do
                  return ()
                Left AbortPositive -> do
                  error "Expected failure"
                Left (TestFailure msg) -> do
                  error ("Test failed because: " ++ msg ++
                         "\n\nOutput of test fixture: " ++ show out ++
                         "\n")
                Right () -> return ()

          let errors = [ e | (ErrorMessage e) <- events]
              expectedErrors = dictEntries "error"
          if errors `eqMessages` expectedErrors
            then return ()
            else fail (
                "Errors did not match. Was:\n" ++ unlines errors ++ "\n" ++
                "Expected:\n" ++ unlines expectedErrors ++
                "Events:\n" ++ show events
              )

        assertOk = case unitOrErr of
                Left Abort -> do
                  error "Unexpected abort"
                Left AbortPositive ->
                  return ()
                Left (TestFailure msg) -> do
                  error ("Test failed because: " ++ msg ++
                         "\n\nOutput of test fixture: " ++ show out ++
                         "\n")
                Right () -> return ()

    if null $ dictEntries "ok"
      then assertError
      else assertOk

newtype Raw = Raw String
  deriving (Eq, Ord)
instance Show Raw where
  show (Raw x) = x
