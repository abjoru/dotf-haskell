{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Instances where

import Core.Types.Types
import Core.Types.Startpage
import Core.Types.Startpage.Instances
import Core.Types.Docker
import Core.Types.Docker.Instances

import Data.Yaml

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
    <*> o .:? "docker"
  parseJSON _ = fail "Expected Object for Config"
