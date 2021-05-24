{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Core.Types.Bundles (
  module Core.Types.Bundles.Types,

  mkUpdateCommands
) where

import qualified Core.Term                as Term

import           Core.Format
import           Core.Os
import           Core.Types.Bundles.Types
import           Core.Types.Types

import           Control.Monad
import           Control.Monad.Extra      (partitionM)

import           Data.String.Interpolate  (i)

import           System.Directory
import           System.FilePath          ((</>))
import           System.Process

loadOsPackages :: PkgSystem -> IO OsPackages
loadOsPackages sys = do
  pkgs <- listOsPackages sys
  return $ OsPackages pkgs
  where listOsPackages Pacman   = map f . lines <$> readProcess "pacman" ["-Q"] []
        listOsPackages Apt      = map f . lines <$> readProcess "apt" ["list", "--installed"] []
        listOsPackages Homebrew = do
          std  <- foldl co [] . lines <$> readProcess "brew" ["list", "--formula"] []
          cask <- foldl co [] . lines <$> readProcess "brew" ["list", "--cask"] []
          return $ std ++ cask

        f line = head $ words line
        co acc line = acc ++ words line

mkUpdateCommands :: Env -> IO [String]
mkUpdateCommands env@(Env sys _ _) =
  buildCommands env =<< loadOsPackages sys

---------------
-- Internals --
---------------

-- Builds install/update commands for all packages (git pkgs included)
buildCommands :: Env -> OsPackages -> IO [String]
buildCommands env op = do
  (refresh, _, install) <- partitionGitPaths env

  return $ mkSystemUpdateCmd env
        ++ (mkScriptCmds env $ extractPreScripts env)
        ++ (mkSystemInstallCmds env $ extractPkgs env op)
        ++ (mkGitInstallCmds env install)
        ++ (mkGitUpdateCmds env refresh)
        ++ (mkScriptCmds env $ extractInstallScripts env)
        ++ (mkScriptCmds env $ extractPostScripts env)

-- Make executable commands for scripts
mkScriptCmds :: Env -> [FilePath] -> [String]
mkScriptCmds (Env _ cfg _) xs = ("bash " ++) <$> abs xs
  where abs :: [FilePath] -> [FilePath]
        abs xs = checkPath (configHomeDirectory cfg) (configDirectory cfg) <$> xs

-- Make Git install commands for missing packages
mkGitInstallCmds :: Env -> [Git] -> [String]
mkGitInstallCmds (Env _ c _) gx = concat (buildCmd (configGitDirectory c) <$> gx)
  where buildCmd :: FilePath -> Git -> [String]
        buildCmd d g@(Git n u Nothing False s c _)  = [[i|git clone #{u} #{targetDir d g}|], inst s c (d </> n)]
        buildCmd d g@(Git n u Nothing True s c _)   = [[i|git clone --recurse-submodules #{u} #{targetDir d g}|], inst s c (d </> n)]
        buildCmd d g@(Git n u (Just b) False s c _) = [[i|git clone -b #{b} #{u} #{targetDir d g}|], inst s c (d </> n)]
        buildCmd d g@(Git n u (Just b) True s c _)  = [[i|git clone ----recurse-submodules -b #{b} #{u} #{targetDir d g}|], inst s c (d </> n)]

        targetDir _ (Git _ _ _ _ _ _ (Just t)) = t
        targetDir c (Git n _ _ _ _ _ _)        = c </> n

        inst _ (Just x) d = mkCmdIn d x
        inst (Just x) _ _ = "bash " ++ x
        inst _ _ d        = [i|echo 'No install instructions for #{d}'|]

-- Make Git update commands for existing packages
mkGitUpdateCmds :: Env -> [Git] -> [String]
mkGitUpdateCmds (Env _ c _) gx = concat (buildCmd (configGitDirectory c) <$> gx)
  where buildCmd :: FilePath -> Git -> [String]
        buildCmd d (Git n _ _ False s c _) = [mkCmdIn (d </> n) "git pull", inst s c (d </> n)]
        buildCmd d (Git n _ _ True s c _)  = [mkCmdIn (d </> n) "git pull && git submodules update --recursive", inst s c (d </> n)]

        inst _ (Just x) d = mkCmdIn d x
        inst (Just x) _ _ = "bash " ++ x
        inst _ _ d        = [i|echo 'No install instructions for #{d}'|]

-- Make update command for system packages
mkSystemUpdateCmd :: Env -> [String]
mkSystemUpdateCmd (Env Pacman _ _)   = ["yay && sudo pacman -Syyu"]
mkSystemUpdateCmd (Env Homebrew _ _) = ["brew upgrade"]
mkSystemUpdateCmd (Env Apt _ _)      = ["sudo apt update && sudo apt upgrade"]

-- Make install commands for system packages
mkSystemInstallCmds :: Env -> [Package] -> [String]
mkSystemInstallCmds (Env Pacman _ _) existing   = mkArchInstallCmds existing
mkSystemInstallCmds (Env Homebrew _ _) existing = mkOsxInstallCmds existing
mkSystemInstallCmds (Env Apt _ _) existing      = mkDebianInstallCmds existing

-- Make Arch install commands
mkArchInstallCmds :: [Package] -> [String]
mkArchInstallCmds px =
  let (pac, aur) = foldl par ([], []) px
      pacCmd = mkString "sudo pacman -S " " " "" pac
      aurCmd = mkString "yay -S " " " "" aur
   in [pacCmd, aurCmd]
  where par (a, b) (Pac n True) = (a, n:b)
        par (a, b) (Pac n _)    = (n:a, b)

-- Make OSx install commands
mkOsxInstallCmds :: [Package] -> [String]
mkOsxInstallCmds px =
  let (brew, bHead, cask) = foldl par ([], [], []) px
      brewCmd = mkString "brew install " " " "" brew
      brewHeadCmd = mkString "brew install --HEAD " " " "" bHead
      caskCmd = mkString "brew install --cask " " " "" cask
   in [brewCmd, brewHeadCmd, caskCmd]
  where par (a, b, c) (Brew n True _) = (a, b, n:c)
        par (a, b, c) (Brew n _ True) = (a, n:b, c)
        par (a, b, c) (Brew n _ _)    = (n:a, b, c)

-- Make Debian install commands
mkDebianInstallCmds :: [Package] -> [String]
mkDebianInstallCmds px =
  let debs = map n px
   in [mkString "sudo apt install " " " "" debs]
  where n (Deb name) = name

-- Partition 'upd' -> 'noop' -> 'new'
partitionGitPaths :: Env -> IO ([Git], [Git], [Git])
partitionGitPaths env@(Env _ c ic) =
  let pkgs = foldl (\a b -> a ++ bundleGitPkgs b) [] $ activeBundles env
   in foldM par ([], [], []) pkgs
  where par (a, b, e) g = do
          upd <- pullRequired $ configGitDirectory c </> gitName g
          exs <- doesDirectoryExist $ configGitDirectory c </> gitName g
          case (upd, exs) of
            (True, True)  -> pure (g:a, b, e)
            (False, True) -> pure (a, g:b, e)
            _             -> pure (a, b, g:e)

-- Extract all packages that has not yet been installed on the system
extractPkgs :: Env -> OsPackages -> [Package]
extractPkgs env (OsPackages sp) =
  foldl coll [] $ activeBundles env
  where coll acc b = acc ++ filter newPkg (bundlePackages b)
        newPkg (Pac n _)    = n `notElem` sp
        newPkg (Brew n _ _) = n `notElem` sp
        newPkg (Deb n)      = n `notElem` sp

-- Extract all install scripts (from all bundles)
extractInstallScripts :: Env -> [FilePath]
extractInstallScripts env = foldl coll [] $ activeBundles env
  where coll acc (Bundle _ _ _ _ (Just s) _ _) = s:acc
        coll acc _                             = acc

-- Extract all pre-install scripts (from all bundles)
extractPreScripts :: Env -> [FilePath]
extractPreScripts env = foldl coll [] $ activeBundles env
  where coll acc (Bundle _ _ _ _ _ (Just s) _) = s:acc
        coll acc _                             = acc

-- Extract all post-install scripts (from all bundles)
extractPostScripts :: Env -> [FilePath]
extractPostScripts env = foldl coll [] $ activeBundles env
  where coll acc (Bundle _ _ _ _ _ _ (Just s)) = s:acc
        coll acc _                             = acc

activeBundles :: Env -> [Bundle]
activeBundles (Env _ c ic) = filter f $ bundles ic
  where f x = configHeadless c == bundleHeadless x
