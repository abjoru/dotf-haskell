{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Docker.Decoders (fixDockerConfig) where

import Core.Os
import Core.Types.Docker.Types
import Core.Types.Docker.Instances

import Data.Yaml

import System.Directory
import System.FilePath ((</>))

fixDockerConfig :: CheckPath -> CheckMaybePath -> Maybe DockerConfig -> Maybe DockerConfig
fixDockerConfig _ _ Nothing = Nothing
fixDockerConfig cp cmp (Just dc) = Just $ dc { dockerVpn = checkVpn cp cmp $ dockerVpn dc
                                             , dockerSystem = checkSystem cp $ dockerSystem dc
                                             , dockerMedia = checkMedia cp $ dockerMedia dc
                                             , dockerEnvOverride = cmp $ dockerEnvOverride dc
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
