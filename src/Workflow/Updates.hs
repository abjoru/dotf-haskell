{-# LANGUAGE QuasiQuotes #-}
module Workflow.Updates (updateSystem) where

import Core.Os
import Core.Types
import Core.Format
import Core.Options

import Control.Monad

import Data.String.Interpolate (i)

import System.Process
import System.Directory (doesPathExist)
import System.FilePath ((</>))

updateSystem :: Env -> DryMode -> IO ()
updateSystem e d = do
  osp  <- loadOsPackages $ pkgSystem e
  cmds <- mkCommands e osp
  mapM_ (run d) $ filter ("" /=) cmds
  where run Dry c = putStrLn c
        run Normal c = system c >> pure ()

-- Creates a list of command strings to execute in order
mkCommands :: Env -> OsPkgs -> IO [String]
mkCommands e osp = do
  gitPkgs <- extractGits e

  return $ (mkUpdateCmd e)
        ++ (mkScriptCmds $ extractPreInstallScripts e)
        ++ (mkPkgCmds e $ extractPkgs e osp)
        ++ (mkGitCmds e gitPkgs)
        ++ (mkPipCmds $ extractPips e osp)
        ++ (mkScriptCmds $ extractInstallScripts e)
        ++ (mkScriptCmds $ extractPostInstallScripts e)

--------------
-- Commands --
--------------

-- Create os dependent update commands
mkUpdateCmd :: Env -> [String]
mkUpdateCmd (Env Pacman _ _)   = ["yay && sudo pacman -Syyu"]
mkUpdateCmd (Env Homebrew _ _) = ["brew upgrade"]
mkUpdateCmd (Env Apt _ _)      = ["sudo apt update && sudo apt upgrade"]

-- Create pkg commands
mkPkgCmds :: Env -> [Pkg] -> [String]
mkPkgCmds _ [] = []
mkPkgCmds (Env ps _ _) px = mkPkgCmds' ps $ foldl par ([], [], []) px
  where par (s, a, c) p@(Pkg _ False False) = (p:s, a, c)
        par (s, a, c) p@(Pkg _ True _)      = (s, p:a, c)
        par (s, a, c) p@(Pkg _ _ True)      = (s, a, p:c)

mkPkgCmds' :: PkgSystem -> ([Pkg], [Pkg], [Pkg]) -> [String]
mkPkgCmds' _ ([], [], [])      = []
mkPkgCmds' Pacman (s, [], _)   = [pacman s]
mkPkgCmds' Pacman ([], a, _)   = [aur a]
mkPkgCmds' Pacman (s, a, _)    = [pacman s, aur a]
mkPkgCmds' Homebrew (s, _, []) = [brew s]
mkPkgCmds' Homebrew ([], _, c) = [cask c]
mkPkgCmds' Homebrew (s, _, c)  = [brew s, cask c]
mkPkgCmds' Apt (s, _, _)       = [apt s]

pacman :: [Pkg] -> String
pacman px = mkString "sudo pacman -S " " " "" $ pkgName <$> px

aur :: [Pkg] -> String
aur px = mkString "yay -S " " " "" $ pkgName <$> px

brew :: [Pkg] -> String
brew px = mkString "brew install" " " "" $ pkgName <$> px

cask :: [Pkg] -> String
cask px = mkString "brew cask install " " " "" $ pkgName <$> px

apt :: [Pkg] -> String
apt px = mkString "sudo apt install " " " "" $ pkgName <$> px

mkGitCmds :: Env -> [GitPkg] -> [String]
mkGitCmds (Env _ c _) gx = concat (buildCmd (gitDirectory c) <$> gx)
  where buildCmd :: FilePath -> GitPkg -> [String]
        buildCmd d (GitPkg n u _ s c) = [[i|git clone #{u} #{d </> n}|], inst s c (d </> n)]

        inst :: Maybe String -> Maybe FilePath -> FilePath -> String
        inst _ (Just x) d = [i|pushd #{d} ; #{x} ; popd|]
        inst (Just x) _ _ = "sh " ++ x
        inst _ _ d        = [i|echo 'No install instructions for #{d}'|]

mkPipCmds :: [String] -> [String]
mkPipCmds xs = [mkString "pip install " " " "" xs]

mkScriptCmds :: [FilePath] -> [String]
mkScriptCmds xs = (\x -> "sh " ++ x) <$> xs

----------------
-- Extractors --
----------------

-- Extract all 'new' packages.
extractPkgs :: Env -> OsPkgs -> [Pkg]
extractPkgs (Env _ _ ic) (OsPkgs sp _) = foldl coll [] $ bundles ic
  where coll acc b = acc ++ (filter newPkg $ bundlePackages b)
        newPkg (Pkg n _ _) = n `notElem` sp

-- Extract all 'non-existant' git paths
extractGits :: Env -> IO [GitPkg]
extractGits (Env _ c ic) =
  let pkgs = foldl (\a b -> a ++ bundleGitPkgs b) [] $ bundles ic
   in filterM exists pkgs
  where exists g = doesPathExist $ gitDirectory c </> gitName g

-- Extract all 'new' pip packages.
extractPips :: Env -> OsPkgs -> [String]
extractPips (Env _ _ ic) (OsPkgs _ pp) = foldl coll [] $ bundles ic
  where coll acc b = acc ++ (filter newPkg $ bundlePipPkgs b)
        newPkg p = p `notElem` pp

-- Extract all install scripts
extractInstallScripts :: Env -> [FilePath]
extractInstallScripts (Env _ _ ic) = foldl coll [] $ bundles ic
  where coll acc (Bundle _ _ _ _ _ (Just s) _ _) = s:acc
        coll acc _                               = acc

-- Extract all pre-install scripts
extractPreInstallScripts :: Env -> [FilePath]
extractPreInstallScripts (Env _ _ ic) = foldl coll [] $ bundles ic
  where coll acc (Bundle _ _ _ _ _ _ (Just s) _) = s:acc
        coll acc _                               = acc

-- Extract all post-install scripts
extractPostInstallScripts :: Env -> [FilePath]
extractPostInstallScripts (Env _ _ ic) = foldl coll [] $ bundles ic
  where coll acc (Bundle _ _ _ _ _ _ _ (Just s)) = s:acc
        coll acc _                               = acc
