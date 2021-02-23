{-# LANGUAGE QuasiQuotes #-}
module Core.Options (
  -- Types
  DryMode(..),
  GitMode(..),
  Options(..),
  Command(..),
  ListCmds(..),
  AddMode(..),
  ListOps(..),
  GenCmds(..),
  ComposeCmds(..),

  -- Functions
  parseOptions
) where

import           Core.Options.Parsers
import           Core.Options.Types

import           Options.Applicative

import           Data.String.Interpolate (__i)

--------------------
-- Options Module --
--------------------

parseOptions :: IO Options
parseOptions = execParser $ info (parserOptions <**> helper)
  (fullDesc <> progDesc [__i|DotF :: The simple dot-file manager.

                             This little application allows for setting up standard applications that
                             should be installed on a new system. It wraps some GIT commands to make
                             it easier to work with the bare repository.
                        |])
