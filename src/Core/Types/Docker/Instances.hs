{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Docker.Instances where

import Core.Types.Docker.Types

import Data.Yaml

instance FromJSON DockerConfig where
  parseJSON (Object o) = DockerConfig
    <$> o .:? "network"
    <*> o .:? "vpn"
    <*> o .:? "system"
    <*> o .:? "media"
    <*> o .:? "env-overrides"
  parseJSON _ = fail "Expected object type for DockerConfig"

instance FromJSON Network where
  parseJSON (Object o) = Network
    <$> o .: "hostname"
    <*> o .: "lan"
    <*> o .: "ns1"
    <*> o .: "ns2"
  parseJSON _ = fail "Expected object type for Network"

instance FromJSON Vpn where
  parseJSON (Object o) = Vpn
    <$> o .: "client"
    <*> o .: "options"
    <*> o .: "config-dir"
    <*> o .: "password"
    <*> o .: "username"
    <*> o .: "provider"
    <*> o .:? "config"
    <*> o .:? "wireguard-dir"
  parseJSON _ = fail "Expected object type for Vpn"

instance FromJSON System where
  parseJSON (Object o) = System
    <$> o .: "download-dir"
    <*> o .: "docker-config-dir"
    <*> o .: "docker-storage-dir"
    <*> o .: "docker-shared-dir"
    <*> o .: "docker-backup-dir"
  parseJSON _ = fail "Expected object type for System"

instance FromJSON Media where
  parseJSON (Object o) = Media
    <$> o .: "tv"
    <*> o .: "books"
    <*> o .: "music"
    <*> o .: "movies"
    <*> o .: "comics"
    <*> o .: "audiobooks"
  parseJSON _ = fail "Expected object type for Media"
