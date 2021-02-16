module Core.Os where

import Core.Utils
import qualified Core.Term as Term

import GHC.Generics

import Data.Aeson
import Data.String.Interpolate (i)
import qualified Data.ByteString.Lazy.UTF8 as BLU

import Core.Types.Bundles.Types

import System.Exit
import System.Process
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))

installFilename :: PkgSystem -> String
installFilename Pacman   = "pacman.yaml"
installFilename Homebrew = "brew.yaml"
installFilename Apt      = "apt.yaml"

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

------------------
-- Git Commands --
------------------

pullRequired :: FilePath -> IO Bool
pullRequired fp = do
  r <- run fp "git fetch ; [ $(git rev-parse HEAD) = $(git rev-parse @{u}) ]"
  case r of
    ExitFailure 1 -> pure True
    _             -> pure False
  where run p cmd = system $ mkCmdIn p cmd

--------------------
-- Shell Commands --
--------------------

checkDependency :: String -> IO ()
checkDependency app = which app >>= check
  where check Nothing = error $ "ERROR: Missing " ++ app ++ " on system path!"
        check _       = pure ()

-- checkDep :: String -> IO (Either String ())
-- checkDep app = check <$> which app
  -- where check Nothing = Left $ "Missing " ++ app ++ " on system path!"
        -- check _       = Right ()

--checkEnv :: Env -> IO ()
--checkEnv (Env _ _ ic) = case pips of
  --[] -> pure ()
  --_  -> checkDependency "pip"
  --where pips = foldl coll [] $ bundles ic
        --coll acc b = acc ++ bundlePipPkgs b

-- Check if prog exists on system
which :: String -> IO (Maybe String)
which arg = do
  x  <- system $ "command -v " ++ arg ++ " >/dev/null"
  case x of
    ExitSuccess   -> pure $ Just arg
    ExitFailure _ -> pure Nothing

-- Flipped version of which. This will return the 
-- app name if the app is not found!
which' :: String -> IO (Maybe String)
which' app = do
  x <- which app
  return $ case x of
    Just _  -> Nothing 
    Nothing -> Just app
