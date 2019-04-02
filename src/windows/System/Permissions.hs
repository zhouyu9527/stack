{-# LANGUAGE NoImplicitPrelude #-}
module System.Permissions
  ( setScriptPerms
  , osIsWindows
  , setFileExecutable
  , runAndKillProcess
  ) where

import System.Process.Typed (ProcessConfig, runProcess)
import Stack.Prelude

-- | True if using Windows OS.
osIsWindows :: Bool
osIsWindows = True

setScriptPerms :: Monad m => FilePath -> m ()
setScriptPerms _ = return ()

setFileExecutable :: Monad m => FilePath -> m ()
setFileExecutable _ = return ()

-- | On Windows, we don't bother with trying to get a
-- SIGKILL. Terminate and hope for the best!
runAndKillProcess :: MonadIO m => ProcessConfig stdin stdout stderr -> m ExitCode
runAndKillProcess = runProcess
