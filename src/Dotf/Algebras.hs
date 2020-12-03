{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dotf.Algebras (
  runUpdate, runStats
) where

import Dotf.Os
import Dotf.Package
import Dotf.Utils
import Dotf.Types
import Dotf.Options (DryRun(..))

import qualified PkgSys.Pacman as PM

import qualified Data.Text as T
import Data.List (intersect, nub, (\\))

import System.Process
import System.Exit (ExitCode, exitSuccess)

class Monad m => Algebra m where
  -- Update (dry?) system
  update :: DryRun -> [Bundle] -> m ()

  -- List managed packages
  stats :: [Bundle] -> m Stats

-- Runs pkg update for this OS
runUpdate :: Config -> DryRun -> [Bundle] -> IO ()
runUpdate (Config _ "pacman") a b = unPacmanAlgebra $ update a b
runUpdate (Config _ "brew") a b   = unBrewAlgebra $ update a b
runUpdate (Config _ "apt") a b    = unAptAlgebra $ update a b
runUpdate _ _ _                   = error "Unsupported package system!"

-- Run show list of pkgs for this OS
runStats :: Config -> [Bundle] -> IO Stats
runStats (Config _ "pacman") xs = unPacmanAlgebra $ stats xs
runStats (Config _ "brew") xs   = unBrewAlgebra $ stats xs
runStats (Config _ "apt") xs    = unAptAlgebra $ stats xs
runStats _ _                    = error "Unsupported package system!"



------------
-- Pacman --
------------

newtype PacmanAlgebra a = PacmanAlgebra { unPacmanAlgebra :: IO a }
  deriving (Monad, Applicative, Functor)

instance Algebra PacmanAlgebra where

  update Dry bundles = do
    sx <- stats bundles
    PacmanAlgebra $ do
      _ <- putStrLn "sudo pacman -Syyu"
      head <$> mapM (PM.updateContent Dry sx) bundles

  update Normal bundles = do
    sx <- stats bundles
    PacmanAlgebra $ do
      _ <- PM.updateSystem
      head <$> mapM (PM.updateContent Normal sx) bundles

  stats xs = PacmanAlgebra $ do
    sp <- PM.listNames
    let a1 = lookupPkgs' (\p -> pacman p) xs
    let a2 = lookupPkgs' (\p -> aur p) xs
    return $ Stats sp a1 a2
    

--------------
-- Homebrew --
--------------

newtype BrewAlgebra a = BrewAlgebra { unBrewAlgebra :: IO a }
  deriving (Monad, Applicative, Functor)

instance Algebra BrewAlgebra where
  update Dry _    = BrewAlgebra $ putStrLn "brew upgrade"
  update Normal _ = BrewAlgebra $ return ()

  stats _ = BrewAlgebra $ return $ Stats [] [] []

---------
-- Apt --
---------

newtype AptAlgebra a = AptAlgebra { unAptAlgebra :: IO a }
  deriving (Monad, Applicative, Functor)

instance Algebra AptAlgebra where
  update _ _ = AptAlgebra $ return ()

  stats _ = AptAlgebra $ return $ Stats [] [] []
