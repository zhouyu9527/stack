{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Utilities for running stack commands.
module Stack.Runners
    ( -- * File locking
      munlockFile
      -- * Runner
    , withRunner
      -- * Config
    , withGlobalConfig
    , withConfig
      -- * BuildConfig
    , withActualBuildConfig
    , withActualBuildConfigAndLock
      -- * EnvConfig
    , withBuildConfig
    , withBuildConfigAndLock
    , withDefaultBuildConfig
    , withDefaultBuildConfigAndLock
    , withBuildConfigExt
    , withBuildConfigDot
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
import           Stack.Dot

-- | Enforce mutual exclusion of every action running via this
-- function, on this path, on this users account.
--
-- A lock file is created inside the given directory.  Currently,
-- stack uses locks per-snapshot.  In the future, stack may refine
-- this to an even more fine-grain locking approach.
--
withUserFileLock :: HasLogFunc env
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
            bracket
              (tryLockFile (toFilePath pth) Exclusive)
              munlockFile
              (\fstTry ->
                  case fstTry of
                    Just lk -> finally (run $ act $ Just lk) (unlockFile lk)
                    Nothing -> do
                      run $ logWarn $
                        "Failed to grab lock (" <> fromString (toFilePath pth) <>
                        "); other stack instance running.  Waiting..."
                      bracket
                        (lockFile (toFilePath pth) Exclusive)
                        unlockFile
                        (\lk -> run $ do
                            logInfo "Lock acquired, proceeding."
                            act $ Just lk))
        else run $ act Nothing

-- | Unlock a lock file, if the value is Just
munlockFile :: MonadIO m => Maybe FileLock -> m ()
munlockFile Nothing = return ()
munlockFile (Just lk) = liftIO $ unlockFile lk

-- NOTE: Functions below are intentionally more monomorphic than they
-- need to be, i.e. they do not use type classes like HasRunner in the
-- signatures. This helps ensure that we don't accidentally do
-- something like `withRunner $ withRunner foo`.

-- | Run the given action with a 'Runner' created from the given
-- 'GlobalOpts'
withRunner :: GlobalOpts -> RIO Runner a -> IO a
withRunner go inner = do
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

-- | Loads global config, ignoring any configuration which would be
-- loaded due to $PWD.
withGlobalConfig :: RIO Config a -> RIO Runner a
withGlobalConfig inner = do
  let modifyGO go = go { globalStackYaml = SYLGlobal }
  local (over globalOptsL modifyGO) $ withConfig inner

withConfig :: RIO Config a -> RIO Runner a
withConfig = withConfigInternal Docker.entrypoint

withActualBuildConfig :: RIO BuildConfig a -> RIO Config a
withActualBuildConfig inner = do
  bconfig <- loadBuildConfig
  runRIO bconfig inner

withActualBuildConfigAndLock
  :: (Maybe FileLock -> RIO BuildConfig a)
  -> RIO Config a
withActualBuildConfigAndLock inner = do
  root <- view stackRootL
  bconfig <- loadBuildConfig
  withUserFileLock root $ runRIO bconfig . inner

withBuildConfig
    :: NeedTargets
    -> BuildOptsCLI
    -> RIO EnvConfig a
    -> RIO BuildConfig a
withBuildConfig needTargets boptsCLI inner =
    withBuildConfigAndLock needTargets boptsCLI (\lk -> do munlockFile lk
                                                           inner)

withBuildConfigAndLock
    :: NeedTargets
    -> BuildOptsCLI
    -> (Maybe FileLock -> RIO EnvConfig a)
    -> RIO BuildConfig a
withBuildConfigAndLock needTargets boptsCLI inner =
    withBuildConfigExt WithDocker needTargets boptsCLI Nothing inner Nothing

-- For now the non-locking version just unlocks immediately.
-- That is, there's still a serialization point.
withDefaultBuildConfig
    :: RIO EnvConfig ()
    -> RIO BuildConfig ()
withDefaultBuildConfig inner =
    withBuildConfigAndLock AllowNoTargets defaultBuildOptsCLI (\lk -> do munlockFile lk
                                                                         inner)

withDefaultBuildConfigAndLock
    :: (Maybe FileLock -> RIO EnvConfig ())
    -> RIO BuildConfig ()
withDefaultBuildConfigAndLock inner =
    withBuildConfigExt WithDocker AllowNoTargets defaultBuildOptsCLI Nothing inner Nothing

withBuildConfigExt
    :: WithDocker
    -> NeedTargets
    -> BuildOptsCLI
    -> Maybe (RIO BuildConfig ())
    -- ^ Action to perform before the build.  This will be run on the host
    -- OS even if Docker is enabled for builds.  The build config is not
    -- available in this action, since that would require build tools to be
    -- installed on the host OS.
    -> (Maybe FileLock -> RIO EnvConfig a)
    -- ^ Action that uses the build config.  If Docker is enabled for builds,
    -- this will be run in a Docker container.
    -> Maybe (RIO BuildConfig ())
    -- ^ Action to perform after the build.  This will be run on the host
    -- OS even if Docker is enabled for builds.  The build config is not
    -- available in this action, since that would require build tools to be
    -- installed on the host OS.
    -> RIO BuildConfig a
withBuildConfigExt skipDocker needTargets boptsCLI mbefore inner mafter = do
  root <- view stackRootL
  withUserFileLock root $ \lk0 -> do
    -- A local bit of state for communication between callbacks:
    curLk <- newIORef lk0
    let inner' lk =
          -- Locking policy:  This is only used for build commands, which
          -- only need to lock the snapshot, not the global lock.  We
          -- trade in the lock here.
          do dir <- installationRootDeps
             -- Hand-over-hand locking:
             withUserFileLock dir $ \lk2 -> do
               writeIORef curLk lk2
               munlockFile lk
               logDebug "Starting to execute command inside EnvConfig"
               inner lk2

    bconfig <- ask
    projectRoot <- view projectRootL
    let inner'' lk = do
            envConfig <- runRIO bconfig $ setupEnv needTargets boptsCLI Nothing
            runRIO envConfig (inner' lk)

    compilerVersion <- view wantedCompilerVersionL
    case skipDocker of
      SkipDocker ->
        sequence_ mbefore *>
        Nix.reexecWithOptionalShell projectRoot compilerVersion (inner'' lk0) <*
        sequence_ mafter
      WithDocker -> Docker.reexecWithOptionalContainer
                      projectRoot
                      mbefore
                      (runRIO bconfig
                        (Nix.reexecWithOptionalShell projectRoot compilerVersion (inner'' lk0)))
                      mafter
                      (Just $
                            do lk' <- readIORef curLk
                               munlockFile lk')

-- Plumbing for --test and --bench flags
withBuildConfigDot
    :: DotOpts
    -> RIO EnvConfig ()
    -> RIO BuildConfig ()
withBuildConfigDot opts f =
    local (over globalOptsL modifyGO) $ withBuildConfig NeedTargets boptsCLI f
  where
    boptsCLI = defaultBuildOptsCLI
        { boptsCLITargets = dotTargets opts
        , boptsCLIFlags = dotFlags opts
        }
    modifyGO =
        (if dotTestTargets opts then set (globalOptsBuildOptsMonoidL.buildOptsMonoidTestsL) (Just True) else id) .
        (if dotBenchTargets opts then set (globalOptsBuildOptsMonoidL.buildOptsMonoidBenchmarksL) (Just True) else id)
