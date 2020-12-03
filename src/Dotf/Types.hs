{-# LANGUAGE OverloadedStrings #-}
module Dotf.Types where

import Data.Yaml

import System.FilePath

data Stats = Stats
  { systemPkgs :: [String]
  , managedPkgs :: [String]
  , specialPkgs :: [String]
  }

data Bundle = ScriptBundle { script :: FilePath }
            | PkgBundle { pre :: Maybe FilePath, post :: Maybe FilePath, pkg :: Package }

data Package = Package
  { name :: String
  , headless :: Bool
  , pacman :: Maybe [String]
  , aur :: Maybe [String]
  , apt :: Maybe [String]
  , brew :: Maybe [String]
  , brewCask :: Maybe [String]
  , git :: Maybe [GitPkg]
  }

data GitPkg = GitPkg
  { url :: String
  , commit :: Maybe String
  }

---------------
-- Instances --
---------------

instance FromJSON Package where
  parseJSON (Object o) = Package
    <$> o .: "name"
    <*> o .: "headless"
    <*> o .:? "pacman"
    <*> o .:? "aur"
    <*> o .:? "apt"
    <*> o .:? "brew"
    <*> o .:? "brew-cask"
    <*> o .:? "git"
  parseJSON _ = fail "Expected Object for Package value"

instance FromJSON GitPkg where
  parseJSON (Object o) = GitPkg
    <$> o .: "url"
    <*> o .:? "commit"
  parseJSON _ = fail "Expected Object for GitPkg value"
