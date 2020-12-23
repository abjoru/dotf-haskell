{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Core.Types.Instances where

import Core.Types.Types
import Core.Types.Parsers

import Data.Yaml

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

instance FromJSON Config where
  parseJSON (Object o) = Config
    <$> o .: "headless"
    <*> o .: "git-target-dir"
    <*> pure "~/"
    <*> pure "~/.config/dotf"
    <*> o .: "dotf-repo-dir"
    <*> o .:? "extensions"
    <*> o .:? "homepage"
    <*> o .:? "network"
    <*> o .:? "vpn"
    <*> o .:? "system"
    <*> o .:? "media"
    <*> o .:? "env-overrides"
  parseJSON _ = fail "Expected Object for Config"

instance FromJSON Homepage where
  parseJSON (Object o) = Homepage 
    <$> o .: "header"
    <*> o .: "footer"
    <*> o .: "links"
    <*> o .: "css"
  parseJSON _ = fail "Expected Object for Homepage"

instance FromJSON Network where
  parseJSON (Object o) = Network 
    <$> o .: "hostname"
    <*> o .: "lan"
    <*> o .: "ns1"
    <*> o .: "ns2"
  parseJSON _ = fail "Expected Object for Network"

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
  parseJSON _ = fail "Expected Object for Vpn"

instance FromJSON System where
  parseJSON (Object o) = System 
    <$> o .: "download-dir"
    <*> o .: "docker-config-dir"
    <*> o .: "docker-storage-dir"
    <*> o .: "docker-shared-dir"
    <*> o .: "docker-backup-dir"
  parseJSON _ = fail "Expected Object for System"

instance FromJSON Media where
  parseJSON (Object o) = Media 
    <$> o .: "tv"
    <*> o .: "books"
    <*> o .: "music"
    <*> o .: "movies"
    <*> o .: "comics"
    <*> o .: "audiobooks"
  parseJSON _ = fail "Expected Object for Media"

instance FromJSON InstallConfig where
  parseJSON (Object o) = case (icName, icDesc) of
    (Just a, Just b) -> InstallConfig <$> parseJSON a <*> parseJSON b <*> bundles
    _                -> fail "Unable to parse config"
    where icName  = HM.lookup "name" o
          icDesc  = HM.lookup "description" o
          bundles = parseBundles o

instance FromJSON Groups where
  parseJSON (Array a) = Groups <$> mapM parseGroup (V.toList a)
  parseJSON _         = fail "Expected Object for Groups"

instance FromJSON PipList

instance FromJSON PipPkg where
  parseJSON (Object o) = PipPkg <$> o .: "name" <*> o .: "version"
  parseJSON _          = fail "Expected Object for PipPkg"
