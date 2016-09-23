{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Helium.Test.Examples( determineTests
                           ) where
import Test.Tasty
import Test.Tasty.HUnit

import System.Directory(getDirectoryContents)
import System.FilePath((</>))
import Data.List
import Control.Applicative
import Control.Monad.RWS
import Control.Monad.Except
import Helium.MonadCompile
import qualified Helium.Compilation
import Helium.StaticAnalysis.Messages.HeliumMessages(showMessage)
import Helium.Main.Args(Option(..), argsToOptions)
import Data.Sequence(Seq, singleton)
import Data.Foldable
import Data.Function(on, (&))
import qualified Lvm.Common.Id as Lvm
import Lvm.Common.Byte
import qualified Data.ByteString as B
import Helium.Main.CompileUtils(makeCoreLib)
import Data.Char(isSpace)
import Control.Arrow((***))

-- Also defined by directory >= 1.2.5.0, December 2015
listDirectory :: FilePath -> IO [FilePath]
listDirectory = fmap (filter (`notElem` [".", ".."])) . getDirectoryContents

data Environment = Environment
  { sources :: [(String, String)]
  , lvmFiles :: [(String, B.ByteString)]
  , options :: [Option]
  }
instance Monoid Environment where
  mempty = Environment
           { sources = []
           , options = []
           , lvmFiles = []
           }
  mappend = curry (Environment
                   <$> attr sources
                   <*> attr lvmFiles
                   <*> attr options
                  )

data Event = Diagnostics String
           | ErrorMessage String
           | WarningMessage String
           | LVMWritten String Bytes
  deriving(Eq)

instance Show Event where
  show (Diagnostics s) = "Message: " ++ s ++ "\n"
  show (ErrorMessage s) = "Error: " ++ s ++ "\n"
  show (WarningMessage s) = "Warning: " ++ s ++ "\n"
  show (LVMWritten s _) = "Produced LVM file: " ++ s ++ "\n"

data Output = Output
  { reverseEvents :: Seq Event
  , filesWritten :: Seq String
  } deriving(Show)

data StopCondition = TestFailure String
                   | Abort
                   | AbortPositive
testFailure :: String -> TestIO a
testFailure = throwError . TestFailure

instance Monoid Output where
  mempty = Output mempty mempty
  mappend = curry (Output <$> attr reverseEvents <*> attr filesWritten)

attr :: Monoid m => (a -> m) -> (a, a) -> m
attr f = uncurry (mappend `on` f)

newtype TestIO a = TestIO { fromTestIO :: ExceptT StopCondition (RWST Environment Output () IO) a }
                   deriving (Functor, Applicative, Monad, MonadWriter Output, MonadReader Environment, MonadIO, MonadError StopCondition)

instance MonadCompile TestIO where
   compilationOptions = options <$> ask
   logMessage msg = tell $ mempty { reverseEvents = singleton (Diagnostics msg) }
   printWarningMessage msg = tell $ mempty { reverseEvents = singleton (WarningMessage msg) }
   printErrorMessage msg = tell $ mempty { reverseEvents = singleton (ErrorMessage msg) }
   abort = throwError Abort
   abortPositive = throwError AbortPositive
   setDebugFileName _name = return () -- logMessage ("debug: filename is " ++ name)
   setDebugDoneModules _done = return () -- logMessage ("debug: done modules is " ++ show done)
   writeFullQualificationFile _ _ = return ()
   readSourceFile name = do env <- ask
                            return (
                              env & sources
                                  & lookup name
                                  & maybe (error$ "Unanticipated read of source file "++ name) id
                                  )
   findLvmFile name = do env <- ask
                         return (
                              env & lvmFiles
                                  & lookup (Lvm.stringFromId name)
                                  & maybe (error$ "Unanticipated read of lvm file "++ show name) B.unpack
                                  )

   readTypingStrategiesFile _name = return Nothing

   doPhaseWithExit _nrOfMsgs _code (_options, _fullName, _doneModules) phase =
     do result <- phase
        case result of
          Left errs ->
            do -- Note that this should print at most nrOfMsgs messages, but for testing we show all.
               mapM_ (printErrorMessage . showMessage) errs
               abort
          Right a ->
            return a

   enterNewPhase p = logMessage ("Entering phase " ++ p)
   writeCoreFile _ _ = testFailure "writeCoreFile not implemented for test"
   writeLvmFile name content = tell $ mempty { reverseEvents = singleton (LVMWritten name content) }

   -- Needs IO. Maybe we can expose a 'pure interface' (à la lvmlib Id.hs...) on a lower level, by
   -- means of unsafePerformIO Lvm.newNameSupply before every core to lvm compilation.
   createNameSupply = liftIO Lvm.newNameSupply
   submitLog _ _ _ = return ()

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
  let dirPath = ("." </> "test" </> "typeClassesInstancesTyping")
  t <- listDirectory dirPath

  coreLvmFiles <- buildCoreLvmFiles

  prelude <- do
    let basedir = "no-basedir"
        fullName = "Prelude.hs"
        doneModules = []
    contents <- readFile ("lib/" </> fullName)

    let env = mempty { sources = [(fullName, contents)]
                     , options = argsToOptions ["--overloading"]
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
                Right () -> return (toList (reverseEvents out))
    let needle (LVMWritten "Prelude.lvm" x) = Just x
        needle _ = Nothing
        collect f = Data.Foldable.foldr (\item more -> f item <|> more) Nothing
    case collect needle events of
      Nothing -> error ("Prelude.lvm not written!\n" ++ show out)
      Just x -> return (B.pack (listFromBytes x))

  let baseEnv = mempty
                { lvmFiles = [("Prelude", prelude)] `mappend` coreLvmFiles
                }

  testCases <- forM (sort t) $ \fileName ->
    case fileName of
      hsFile | ".hs" `isSuffixOf` hsFile -> fmap (:[]) $ mkTestCase baseEnv dirPath hsFile
      _ -> return []

  return $ testGroup "typeClassesInstancesTyping" $ concat testCases

parseDict :: String -> [(String, String)]
parseDict =
  let pfx = "--- "
      toKeyValue = (triml *** (triml1 . safeTail)) . span (/= ':')
      safeTail (_:tl) = tl
      safeTail [] = []
      mergeLines (l:('|':' ':m):ls) = mergeLines ((l ++ '\n' : m) : ls)
      mergeLines (n:ns) = n : mergeLines ns
      mergeLines [] = []
  in map toKeyValue . mergeLines . map (drop (length pfx)) . filter (isPrefixOf pfx) . lines

triml, trimr, triml1 :: String -> String
triml = dropWhile isSpace
trimr = dropWhileEnd isSpace
triml1 (s:x) | isSpace s = x
triml1 x = x

-- | String equality modulo trailing spaces modulo empty lines
eqMessages :: [String] -> [String] -> Bool
eqMessages = (==) `on` (noEmptyLines . map trimr . normalizeLines)
  where normalizeLines = lines . unlines
        noEmptyLines = filter (not . null)

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

    let events = reverse (toList (reverseEvents out))

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
