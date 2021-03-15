{-# LANGUAGE QuasiQuotes #-}
module Core.Templates where

import           Core.Types

import           Data.Char               (toLower)
import           Data.String.Interpolate (__i, i)

import qualified Data.HashMap.Lazy       as HML

defaultConfig :: Config -> String
defaultConfig (Config h gd _ _ rd _ _ _) =
  [__i|\# Default DotF Configuration

  \#\#\#\#\#\#\#\#\#\#\#
  \# General \#
  \#\#\#\#\#\#\#\#\#\#\#

  \# UI or non-UI system
  headless: #{Prelude.map toLower $ show h}

  \# Bundle extras
  \# Ability to provide extra bundles for server builds
  \# by extending the standard package file. The idea is
  \# to add server modules to this file that you would not
  \# want to have in a desktop package file.
  \#extensions: 'opt-server-ext.yaml'

  \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
  \# Directories \#
  \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

  \# Target directory for git package installs
  git-target-dir: '#{gd}'

  \# Git bare repo directory for dot-files
  dotf-repo-dir: '#{rd}'

  \#\#\#\#\#\#\#\#\#\#
  \# Docker \#
  \#\#\#\#\#\#\#\#\#\#

  \# [Optional] Docker node
  \#docker:

  \#  network:
  \#    hostname: 'somename'
  \#    lan: '10.0.0.0/16'
  \#    ns1: '1.1.1.1'
  \#    ns2: '8.8.8.8'

  \#  vpn:
  \#    client: 'openvpn'
  \#    options: ''
  \#    config-dir: 'some/openvpn/config/dir'
  \#    password: 'secret'
  \#    username: 'me'
  \#    provider: 'CUSTOM'
  \#    config: 'us_loc'
  \#    wireguard-dir: 'some/wireguard/dir'

  \#  system:
  \#    download-dir: 'some/download/dir'
  \#    docker-config-dir: 'some/docker/config/dir'
  \#    docker-storage-dir: 'some/docker/storage/dir'
  \#    docker-shared-dir: 'some/docker/shared/dir'
  \#    docker-backup-dir: 'some/docker/backup/dir'

  \#  media:
  \#    tv: 'some/tv/folder'
  \#    books: 'some/books/folder'
  \#    music: 'some/music/folder'
  \#    movies: 'some/movies/folder'
  \#    comics: 'some/comics/folder'
  \#    audiobooks: 'some/audiobooks/folder'
  |]

defaultPkgConfig :: String
defaultPkgConfig =
  [__i|\# Default package configuration file.
  \#
  \# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  \# This file should be renamed (excluding the 'example-'
  \# part to be picked up by DotF. Note that the filename
  \# is OS dependent, and should follow these patterns:
  \#
  \# - Arch Linux: pacman.yaml
  \# - Debian Linux: apt.yaml
  \# - Mac OSX: brew.yaml
  \# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  \# Meaningless name for this config
  name: "My Packages"

  \# Meaningless description for this config
  description: "Package config for target OS"

  \# Bundles
  \# ~~~~~~~
  \# Bundles groups packages together into a logical unit
  \# You can define multiple bundles such as bundles for
  \# 'internet', 'development', 'desktop', etc.
  \#
  \# The following example illustrates some config options:
  \#
  \# internet:
  \#   \# [Optional] Requires X libs
  \#   headless: false
  \#
  \#   \# [Optional] Non-pkg applications can be installed using
  \#   \# the custom installer script. Default root for all
  \#   \# scripts is the 'dotf-config-dir'.
  \#   script: 'scripts/internet/install.sh'
  \#
  \#   \# [Optional] Any pre-install actions can be defined here
  \#   pre-install: 'scripts/internet/pre-install.sh'
  \#
  \#   \# [Optional] Any post-install actions can be defined here.
  \#   post-install: 'scripts/internet/post-install.sh'
  \#
  \#   \# Packages for this module. Simple declarations
  \#   \# assumes standard packaging (i.e. pacman, apt, brew)
  \#   packages:
  \#   - firefox
  \#   - thunderbird
  \#   \# This package must be fetched from the AUR.
  \#   \# Sample principle applies for 'cask' and 'head' on OSX.
  \#   - rare-pkg:
  \#     aur: true
  \#
  \#   \# Applications from GIT repos can be defined here.
  \#   \# These applicatons will be processed after the
  \#   \# packages defined in the 'packages' section.
  \#   git:
  \#   \# Some arbitrary name for the package
  \#   - some-git-app:
  \#     \# The URL to the GIT hosted application
  \#     \# Note that a git entry should have one of 'command'
  \#     \# or 'install' options defined, otherwise you would
  \#     \# just get a clone and nothing else.
  \#     url: 'https://github.com/<user>/some-git-app.git'
  \#     \# [Optional] Checkout a specific branch.
  \#     \# branch: some-branch
  \#     \# [Optional] Recurse submodules for this repository.
  \#     \# submodules: true
  \#     \# [Optional] Installation command. This command will be
  \#     \# executed from the clone directory (i.e. local to the app)
  \#     command: 'stack install'
  \#     \# [Optional] Install script if 'command' won't cut it.
  \#     \# Note that command takes precedence.
  \#     \# install: 'script/internet/some-git-app.install.sh'
  \#     \# [Optional] Install directory of git sources
  \#     \# target: '/some/path'
  |]
