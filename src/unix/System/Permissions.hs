{-# LANGUAGE NoImplicitPrelude #-}
module System.Permissions
  ( setScriptPerms
  , osIsWindows
  , setFileExecutable
  , runAndKillProcess
  ) where

import qualified System.Posix.Files as Posix
import Stack.Prelude
import System.Process.Internals (ProcessHandle__ (..), withProcessHandle)
import System.Process.Typed
import System.Posix.Signals (signalProcess, sigKILL)
import qualified Control.Exception

-- | True if using Windows OS.
osIsWindows :: Bool
osIsWindows = False

setScriptPerms :: MonadIO m => FilePath -> m ()
setScriptPerms fp = do
    liftIO $ Posix.setFileMode fp $
        Posix.ownerReadMode `Posix.unionFileModes`
        Posix.ownerWriteMode `Posix.unionFileModes`
        Posix.groupReadMode `Posix.unionFileModes`
        Posix.otherReadMode

setFileExecutable :: MonadIO m => FilePath -> m ()
setFileExecutable fp = liftIO $ Posix.setFileMode fp 0o755

-- | Run a process and ensure that it fully dies before returning,
-- sending a SIGKILL as necessary.
runAndKillProcess :: MonadIO m => ProcessConfig stdin stdout stderr -> m ExitCode
runAndKillProcess config = liftIO $ Control.Exception.uninterruptibleMask $ \restore -> do
  p <- startProcess config
  -- First check if it's dead
  eec <- Control.Exception.try $ restore $ waitExitCode p
  case eec :: Either SomeException ExitCode of
    Right ec -> pure ec -- it's dead Jim
    Left e -> do
      stopProcess p

      -- We would like to delay
      -- Should we send the process a SIGKILL
      sendKillRef <- newIORef True

      -- Fork a thread which will send that SIGKILL if needed
      let killer = do
            --threadDelay $ 5 * 1000 * 1000
            sendKill <- readIORef sendKillRef
            when sendKill $ do
              withProcessHandle (unsafeProcessHandle p) $ \p_ -> do
                case p_ of
                  ClosedHandle _ -> pure () -- again, it's dead jim
                  OpenExtHandle{} -> error "OpenExtHandle cannot happen on POSIX"
                  OpenHandle h -> signalProcess sigKILL h
      concurrently_ killer (waitExitCode p *> writeIORef sendKillRef False)
      Control.Exception.throwIO e
