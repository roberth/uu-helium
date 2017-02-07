{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Provide a simple in-memory environment for testing the compiler
module Helium.Test.TestIO
  ( -- * Inputs
    Environment(..)
    -- * Outputs
  , Event(..)
  , Output(..)
    -- * Piece de resistance
  , TestIO
  , fromTestIO
    -- * Control
  , StopCondition(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.RWS
import qualified Data.ByteString                               as B
import           Data.Function                                 ((&))
import           Data.Sequence                                 (Seq, singleton)
import           Data.Foldable                                 (toList)
import           Helium.Main.Args                              (Option (..))
import           Helium.MonadCompile
import           Helium.StaticAnalysis.Messages.HeliumMessages (showMessage)
import           Helium.Utils.Utils                            (mappendOn)
import           Lvm.Common.Byte
import qualified Lvm.Common.Id                                 as Lvm

data Environment = Environment
  { sources  :: [(String, String)]
  , lvmFiles :: [(String, B.ByteString)]
  , options  :: [Option]
  }
instance Monoid Environment where
  mempty = Environment
           { sources = []
           , options = []
           , lvmFiles = []
           }
  mappend = curry (Environment
                   <$> mappendOn sources
                   <*> mappendOn lvmFiles
                   <*> mappendOn options
                  )

data Event = Diagnostics String
           | ErrorMessage String
           | WarningMessage String
           | LVMWritten String Bytes
  deriving(Eq)

instance Show Event where
  show (Diagnostics s)    = "Message: " ++ s ++ "\n"
  show (ErrorMessage s)   = "Error: " ++ s ++ "\n"
  show (WarningMessage s) = "Warning: " ++ s ++ "\n"
  show (LVMWritten s _)   = "Produced LVM file: " ++ s ++ "\n"

data Output = Output
  { outputEvents :: Seq Event
  , filesWritten  :: Seq String
  }
instance Show Output where
  show o = "Output Events: "
    ++ show (toList (outputEvents o))
    ++ "Files written: "
    ++ show (filesWritten o)


data StopCondition = TestFailure String
                   | Abort
                   | AbortPositive
testFailure :: String -> TestIO a
testFailure = throwError . TestFailure

instance Monoid Output where
  mempty = Output mempty mempty
  mappend = curry (Output <$> mappendOn outputEvents <*> mappendOn filesWritten)

newtype TestIO a = TestIO { fromTestIO :: ExceptT StopCondition (RWST Environment Output () IO) a }
                   deriving (Functor, Applicative, Monad, MonadWriter Output, MonadReader Environment, MonadIO, MonadError StopCondition)

instance MonadCompile TestIO where
   compilationOptions = options <$> ask
   logMessage msg = tell $ mempty { outputEvents = singleton (Diagnostics msg) }
   printWarningMessage msg = tell $ mempty { outputEvents = singleton (WarningMessage msg) }
   printErrorMessage msg = tell $ mempty { outputEvents = singleton (ErrorMessage msg) }
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

   enterNewPhase p = logMessage ("Entering phase " ++ p)
   writeCoreFile _ _ = testFailure "writeCoreFile not implemented for test"
   writeLvmFile name content = tell $ mempty { outputEvents = singleton (LVMWritten name content) }

   -- Needs IO. Maybe we can expose a 'pure interface' (à la lvmlib Id.hs...) on a lower level, by
   -- means of unsafePerformIO Lvm.newNameSupply before every core to lvm compilation.
   createNameSupply = liftIO Lvm.newNameSupply
   submitLog _ _ _ = return ()
