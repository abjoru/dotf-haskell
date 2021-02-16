{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Docker.Decoders where

import Core.Utils
import Core.Types.Docker.Types
import Core.Types.Docker.Instances

import Data.Yaml

import System.Directory
import System.FilePath ((</>))

checkDockerConfig :: Maybe DockerConfig -> IO (Maybe DockerConfig)
checkDockerConfig Nothing  = pure Nothing
checkDockerConfig (Just v) = do
  home <- getHomeDirectory
  dotf <- getXdgDirectory XdgConfig "dotf"

  let cp  = checkPath home dotf
      cmp = checkMaybePath home dotf

  return $ Just v { dockerVpn = checkVpn cp cmp $ dockerVpn v
                  , dockerSystem = checkSystem cp $ dockerSystem v
                  , dockerMedia = checkMedia cp $ dockerMedia v
                  , dockerEnvOverride = cmp $ dockerEnvOverride v
                  }

checkVpn :: CheckPath -> CheckMaybePath -> Maybe Vpn -> Maybe Vpn
checkVpn _ _ Nothing     = Nothing
checkVpn cp cmp (Just v) = Just v { vpnConfigDir = cp $ vpnConfigDir v
                                  , vpnWireguardDir = cmp $ vpnWireguardDir v
                                  }

checkSystem :: CheckPath -> Maybe System -> Maybe System
checkSystem _ Nothing   = Nothing
checkSystem cp (Just v) = Just v { systemDownloadDir = cp $ systemDownloadDir v
                                 , systemDockerConfigDir = cp $ systemDockerConfigDir v
                                 , systemDockerStorageDir = cp $ systemDockerStorageDir v
                                 , systemDockerSharedDir = cp $ systemDockerSharedDir v
                                 , systemDockerBackupDir = cp $ systemDockerBackupDir v
                                 }

checkMedia :: CheckPath -> Maybe Media -> Maybe Media
checkMedia _ Nothing   = Nothing
checkMedia cp (Just v) = Just v { mediaTvDir = cp $ mediaTvDir v
                                , mediaBooksDir = cp $ mediaBooksDir v
                                , mediaMusicDir = cp $ mediaMusicDir v
                                , mediaMoviesDir = cp $ mediaMoviesDir v
                                , mediaComicsDir = cp $ mediaComicsDir v
                                , mediaAudiobooksDir = cp $ mediaAudiobooksDir v
                                }
