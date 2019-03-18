{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for running stack commands.
module Stack.Runners
    ( withNoProject
    , withEnvConfigAndLock
    , withDefaultEnvConfigAndLock
    , withBuildConfig
    , withEnvConfig
    , withDefaultEnvConfig
    , withConfig
    , withUserFileLock
    , munlockFile
    , withRunnerGlobal
    ) where

import           Stack.Prelude
import           Path
import           Path.IO
import           RIO.Process (mkDefaultProcessContext)
import           Stack.Build.Target(NeedTargets(..))
import           Stack.Config
import           Stack.Constants
import           Stack.DefaultColorWhen (defaultColorWhen)
import qualified Stack.Docker as Docker
import qualified Stack.Nix as Nix
import           Stack.Setup
import           Stack.Types.Config
import           System.Console.ANSI (hSupportsANSIWithoutEmulation)
import           System.Environment (getEnvironment)
import           System.FileLock
import           System.Terminal (getTerminalWidth)

-- | Enforce mutual exclusion of every action running via this
-- function, on this path, on this users account.
--
-- A lock file is created inside the given directory.  Currently,
-- stack uses locks per-snapshot.  In the future, stack may refine
-- this to an even more fine-grain locking approach.
--
withUserFileLock :: HasRunner env
                 => Path Abs Dir
                 -> (Maybe FileLock -> RIO env a)
                 -> RIO env a
withUserFileLock dir act = withRunInIO $ \run -> do
    env <- getEnvironment
    let toLock = lookup "STACK_LOCK" env == Just "true"
    if toLock
        then do
            let lockfile = relFileLockfile
            let pth = dir </> lockfile
            ensureDir dir
            -- Just in case of asynchronous exceptions, we need to be careful
            -- when using tryLockFile here:
            bracket (tryLockFile (toFilePath pth) Exclusive)
                    munlockFile
                    (\fstTry ->
                        case fstTry of
                          Just lk -> run $ act $ Just lk
                          Nothing ->
                            do run $ logError $
                                 "Failed to grab lock (" <>
                                 displayShow pth <>
                                 "); other stack instance running.  Waiting..."
                               bracket (lockFile (toFilePath pth) Exclusive)
                                       unlockFile
                                       (\lk -> run $ do
                                            logError "Lock acquired, proceeding."
                                            act $ Just lk))
        else run $ act Nothing

-- | Modify the StackYamlLoc to require no project.
withNoProject :: RIO Runner a -> RIO Runner a
withNoProject inner = do
  loc <- view stackYamlLocL
  case loc of
    SYLDefault -> pure ()
    SYLOverride _ -> logWarn "Ignoring override stack.yaml location"
    SYLNoConfig _ -> logWarn "Ignoring override no-config run"
    SYLNoProject -> pure ()

  local (set stackYamlLocL SYLNoProject) inner

-- For now the non-locking version just unlocks immediately.
-- That is, there's still a serialization point.
withDefaultEnvConfig
    :: RIO EnvConfig a
    -> RIO Config a
withDefaultEnvConfig inner =
    withEnvConfigAndLock AllowNoTargets defaultBuildOptsCLI (\lk -> do munlockFile lk
                                                                       inner)

withEnvConfig
    :: NeedTargets
    -> BuildOptsCLI
    -> RIO EnvConfig a
    -> RIO Config a
withEnvConfig needTargets boptsCLI inner =
    withEnvConfigAndLock needTargets boptsCLI (\lk -> do munlockFile lk
                                                         inner)

withDefaultEnvConfigAndLock
    :: (Maybe FileLock -> RIO EnvConfig a)
    -> RIO Config a
withDefaultEnvConfigAndLock = withEnvConfigAndLock AllowNoTargets defaultBuildOptsCLI

withBuildConfig :: RIO BuildConfig a -> RIO Config a
withBuildConfig inner = do
  bconfig <- loadBuildConfig
  runRIO bconfig inner

withEnvConfigAndLock
    :: NeedTargets
    -> BuildOptsCLI
    -> (Maybe FileLock -> RIO EnvConfig a)
    -- ^ Action that uses the build config.  If Docker is enabled for builds,
    -- this will be run in a Docker container.
    -> RIO Config a
withEnvConfigAndLock needTargets boptsCLI inner = do
    config <- ask
    withUserFileLock (view stackRootL config) $ \lkUser ->
      Docker.reexecWithOptionalContainer Nothing lkUser $
      Nix.reexecWithOptionalShell $ withBuildConfig $ do
        envConfig <- setupEnv needTargets boptsCLI Nothing
        runRIO envConfig $ do
          -- Locking policy:  This is only used for build commands, which
          -- only need to lock the snapshot, not the global lock.  We
          -- trade in the lock here.
          dir <- installationRootDeps
          -- Hand-over-hand locking:
          withUserFileLock dir $ \lkSnapshot -> do
            munlockFile lkUser
            logDebug "Starting to execute command inside EnvConfig"
            inner lkSnapshot

-- | Load the configuration. Convenience function used
-- throughout this module.
withConfig
  :: RIO Config a
  -> RIO Runner a
withConfig inner =
    loadConfigInternal $ \config -> do
      -- If we have been relaunched in a Docker container, perform in-container initialization
      -- (switch UID, etc.).  We do this after first loading the configuration since it must
      -- happen ASAP but needs a configuration.
      view (globalOptsL.to globalDockerEntrypoint) >>=
        traverse_ (Docker.entrypoint config)
      runRIO config inner

withRunnerGlobal :: GlobalOpts -> RIO Runner a -> IO a
withRunnerGlobal go inner = do
  colorWhen <-
    case getFirst $ configMonoidColorWhen $ globalConfigMonoid go of
      Nothing -> defaultColorWhen
      Just colorWhen -> pure colorWhen
  useColor <- case colorWhen of
    ColorNever -> return False
    ColorAlways -> return True
    ColorAuto -> fromMaybe True <$>
                          hSupportsANSIWithoutEmulation stderr
  termWidth <- clipWidth <$> maybe (fromMaybe defaultTerminalWidth
                                    <$> getTerminalWidth)
                                   pure (globalTermWidth go)
  menv <- mkDefaultProcessContext
  logOptions0 <- logOptionsHandle stderr False
  let logOptions
        = setLogUseColor useColor
        $ setLogUseTime (globalTimeInLog go)
        $ setLogMinLevel (globalLogLevel go)
        $ setLogVerboseFormat (globalLogLevel go <= LevelDebug)
        $ setLogTerminal (globalTerminal go)
          logOptions0
  withLogFunc logOptions $ \logFunc -> runRIO Runner
    { runnerGlobalOpts = go
    , runnerUseColor = useColor
    , runnerLogFunc = logFunc
    , runnerTermWidth = termWidth
    , runnerProcessContext = menv
    } inner
  where clipWidth w
          | w < minTerminalWidth = minTerminalWidth
          | w > maxTerminalWidth = maxTerminalWidth
          | otherwise = w

-- | Unlock a lock file, if the value is Just
munlockFile :: MonadIO m => Maybe FileLock -> m ()
munlockFile Nothing = return ()
munlockFile (Just lk) = liftIO $ unlockFile lk
