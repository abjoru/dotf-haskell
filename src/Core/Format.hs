{-# LANGUAGE QuasiQuotes #-}
module Core.Format where

import Core.Types

import Data.Yaml
import Data.Char (toLower)
import Data.String.Interpolate (__i)

-- Create a string from an array using the given separator
sep :: String -> [String] -> String
sep _ [] = ""
sep _ [x] = x
sep s (x:xs) = x ++ s ++ sep s xs

-- Create a string from an array using the given separator
-- and pre and post string segments
mkString :: String -> String -> String -> [String] -> String
mkString _ _ _ [] = ""
mkString pre _ post [x] = pre ++ x ++ post
mkString pre s post (x:xs) = pre ++ x ++ s ++ sep s xs ++ post

-- TODO Move to templates, or something

defaultConfig :: Config -> String
defaultConfig (Config h gd _ rd _ _ _ _) =
  [__i|# Default DotF Configuration

  ###########
  # General #
  ###########

  # UI or non-UI system
  headless: #{map toLower $ show h}

  ###############
  # Directories #
  ###############

  # Target directory for git package installs
  git-target-dir: '#{gd}'

  # Git bare repo directory for dot-files
  dotf-repo-dir: '#{rd}'
  |]

defaultPkgConfig :: String
defaultPkgConfig =
  [__i|# Default package configuration file.
  #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This file should be renamed (excluding the 'example-'
  # part to be picked up by DotF. Note that the filename
  # is OS dependent, and should follow these patterns:
  #
  # - Arch Linux: pacman.yaml
  # - Debian Linux: apt.yaml
  # - Mac OSX: brew.yaml
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Meaningless name for this config
  name: "My Packages"

  # Meaningless description for this config
  description: "Package config for target OS"

  # Bundles
  # ~~~~~~~
  # Bundles groups packages together into a logical unit
  # You can define multiple bundles such as bundles for
  # 'internet', 'development', 'desktop', etc.
  #
  # The following example illustrates some config options:
  #
  # internet:
  #   # [Optional] Requires X libs
  #   headless: false
  #
  #   # [Optional] Non-pkg applications can be installed using 
  #   # the custom installer script. Default root for all 
  #   # scripts is the 'dotf-config-dir'.
  #   script: 'scripts/internet/install.sh'
  #
  #   # [Optional] Any pre-install actions can be defined here
  #   pre-install: 'scripts/internet/pre-install.sh'
  #
  #   # [Optional] Any post-install actions can be defined here.
  #   post-install: 'scripts/internet/post-install.sh'
  #
  #   # Packages for this module. Simple declarations
  #   # assumes standard packaging (i.e. pacman, apt, brew)
  #   packages:
  #   - firefox
  #   - thunderbird
  #   # This package must be fetched from the AUR.
  #   # Sample principle applies for 'cask' on OSX.
  #   - rare-pkg:
  #     aur: true
  #
  #   # Applications from GIT repos can be defined here.
  #   # These applicatons will be processed after the 
  #   # packages defined in the 'packages' section.
  #   git:
  #   # Some arbitrary name for the package
  #   - some-git-app:
  #     # The URL to the GIT hosted application
  #     # Note that a git entry should have one of 'command'
  #     # or 'install' options defined, otherwise you would
  #     # just get a clone and nothing else.
  #     url: 'https://github.com/test/some-git-app.git'
  #     # [Optional] Installation command. This command will be 
  #     # executed from the clone directory (i.e. local to the app)
  #     command: 'stack install'
  #     # [Optional] Install script if 'command' won't cut it.
  #     # Note that command takes precedence. 
  #     # install: 'script/internet/some-git-app.install.sh'
  #
  #   # Python packages managed with pip. Requires pip to be 
  #   # installed on the system.
  #   pip:
  #   # Some python package
  #   - neovim
  |]
