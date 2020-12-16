{-# LANGUAGE OverloadedStrings, DeriveGeneric, QuasiQuotes #-}
module Core.Os where

import GHC.Generics

import Data.Aeson
import Data.String.Interpolate (i, __i)
import qualified Data.ByteString.Lazy.UTF8 as BLU

import Core.Types

import System.Exit
import System.Process
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))

installFile :: PkgSystem -> String
installFile Pacman   = "pacman.yaml"
installFile Homebrew = "brew.yaml"
installFile Apt      = "apt.yaml"

-- Attempt to resolve the local package system
-- Errors if no supported package system found!
findPackageSystem :: IO PkgSystem
findPackageSystem = do
  mPacman <- which "pacman"
  mBrew   <- which "brew"
  mApt    <- which "apt"

  case (mPacman, mBrew, mApt) of
    (Just _, _, _) -> return Pacman
    (_, Just _, _) -> return Homebrew
    (_, _, Just _) -> return Apt
    _              -> error "No package system found! Supported systems: pacman, apt, homebrew"

-- Load install config for the given package system
-- Errors if install config for package system was not found!
loadInstallConfig :: PkgSystem -> Config -> IO InstallConfig
loadInstallConfig ps c = do
  rs <- decodeBundles c $ configDirectory c </> installFile ps

  case rs of
    Right x -> return x
    Left err -> do 
      putStrLn [i|Error reading #{installFile ps}! #{show err}|]
      return $ InstallConfig "Empty" "" []

loadDotfConfig :: IO Config
loadDotfConfig = do
  cd <- getXdgDirectory XdgConfig "dotf"
  rs <- decodeConfig $ cd </> "dotf.yaml"

  case rs of
    Right x -> return x
    _       -> error $ "Unable to load config: " ++ (cd </> "dotf.yaml")

loadOsPackages :: PkgSystem -> IO OsPkgs
loadOsPackages ps = do
  pkgs <- listOsPkgs ps
  pips <- listPipPkgs
  return $ OsPkgs pkgs pips

--------------------
-- Shell Commands --
--------------------

checkDependency :: String -> IO ()
checkDependency app = which app >>= check
  where check Nothing = error $ "ERROR: Missing " ++ app ++ " on system path!"
        check _       = pure ()

checkEnv :: Env -> IO ()
checkEnv (Env _ _ ic) = case pips of
  [] -> pure ()
  _  -> checkDependency "pip"
  where pips = foldl coll [] $ bundles ic
        coll acc b = acc ++ bundlePipPkgs b

-- Check if prog exists on system
which :: String -> IO (Maybe String)
which arg = do
  x  <- system $ "command -v " ++ arg ++ " >/dev/null"
  case x of
    ExitSuccess   -> pure $ Just arg
    ExitFailure _ -> pure Nothing

-----------------
-- OS Packages --
-----------------

listOsPkgs :: PkgSystem -> IO [String]
listOsPkgs Pacman = map f . lines <$> readProcess "pacman" ["-Q"] []
  where f line = head $ words line
listOsPkgs Homebrew = do
  std  <- listStd
  cask <- listCask
  return $ std ++ cask
  where listStd = foldl f [] . lines <$> readProcess "brew" ["list", "--formula"] []
        listCask = foldl f [] . lines <$> readProcess "brew" ["list", "--cask"] []
        f acc l = acc ++ words l
listOsPkgs Apt = map f . lines <$> readProcess "apt" ["list", "--installed"] []
  where f line = head $ words line

------------------
-- Pip Packages --
------------------

data PipList = PipList [PipPkg]
  deriving (Show, Generic)

instance FromJSON PipList

data PipPkg = PipPkg { name :: String, version :: String }
  deriving (Show, Generic)

instance FromJSON PipPkg

listPipPkgs :: IO [String]
listPipPkgs = do
  json <- readProcess "pip" ["list", "--format", "json"] ""
  let bs = BLU.fromString json
  case (decode bs :: Maybe PipList) of
    Just (PipList xs) -> return $ f <$> xs
    Nothing -> pure []
  where f (PipPkg n _) = n
