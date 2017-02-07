{-| Module      :  CompileIO
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.CompileIO
  ( runCompileIO
  , CompileIO
  , CompileEnv(..)
  ) where
import Helium.MonadCompile
import Control.Monad.Reader
import Data.IORef
import Helium.Main.CompileUtils
import Helium.StaticAnalysis.Messages.Messages(HasMessage, sortMessages)
import Helium.Utils.Utils(refToCurrentFileName, refToCurrentImported)
import qualified Helium.Utils.Utils as Utils
import System.Directory      (doesFileExist)
import qualified Control.Exception as CE (catch, IOException)
import Lvm.Common.Id         (newNameSupply, stringFromId)
import Lvm.Common.Byte       (writeBytes)
import Lvm.Path (searchPath)
import qualified Data.ByteString as B

data CompileEnv = CompileEnv { compileEnvOptions :: [Option]
                             , compileEnvLvmPath :: [String]
                             }
newtype CompileIO a = CompileIO { fromCompileIO :: ReaderT CompileEnv IO a }

runCompileIO :: CompileIO a -> CompileEnv -> IO a
runCompileIO (CompileIO m) = runReaderT m

-- These instances could be derived instead
instance Functor CompileIO where
  fmap f (CompileIO x) = CompileIO (fmap f x)
instance Applicative CompileIO where
  pure = CompileIO . pure
  (CompileIO f) <*> (CompileIO a) = CompileIO (f <*> a)
instance Monad CompileIO where
  return = CompileIO . return
  (CompileIO m) >>= f = CompileIO (m >>= (fromCompileIO . f))

instance MonadIO CompileIO where
  liftIO = CompileIO . liftIO

instance MonadCompile CompileIO where
  logMessage = liftIO . putStrLn

  printWarningMessage = liftIO . putStr
  printErrorMessage = liftIO . putStr

  abort = liftIO (exitWith (ExitFailure 1))
  abortPositive = liftIO (exitWith (ExitSuccess))

  -- Store the current module file-name and its context in
  -- two IO refs (unsafe! only used for internal error bug-report)
  setDebugFileName = liftIO . writeIORef refToCurrentFileName
  setDebugDoneModules = liftIO . writeIORef refToCurrentImported




  readSourceFile = liftIO . Utils.readSourceFile

  compilationOptions = compileEnvOptions <$> CompileIO ask

  readTypingStrategiesFile name = liftIO $ do
    exists <- doesFileExist name
    when' exists (readFile name)

  writeCoreFile name content = liftIO $ writeFile name content
  writeFullQualificationFile name content = liftIO $ writeFile name content

  findLvmFile ident = do
    lvmPath <- compileEnvLvmPath <$> CompileIO ask
    path <- liftIO $ searchPath lvmPath ".lvm" $ (stringFromId ident)
    liftIO $ fmap B.unpack $ B.readFile path

  writeLvmFile name content = liftIO $ do
     (writeBytes name content) `CE.catch` (\ioErr -> do
        putStrLn ("Could not write to file '" ++
            name ++ "'" ++ show (ioErr :: CE.IOException));
        exitWith (ExitFailure 1)
                                         )
  createNameSupply = liftIO newNameSupply

  submitLog code name modules = (liftIO . sendLog code name modules) =<< compilationOptions

