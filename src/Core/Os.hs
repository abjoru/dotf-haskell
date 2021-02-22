{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Core.Os where

--import Core.Utils
import qualified Core.Term                 as Term

import           GHC.Generics

import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.String.Interpolate   (i)

import           Core.Types.Bundles.Types

import           Control.Monad

import           System.Directory
import           System.Exit
import           System.FilePath           ((</>))
import           System.Process

--------------------
-- Package System --
--------------------

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
  exists <- doesDirectoryExist (fp </> ".git")
  if exists
     then run <$> runCmdIn fp "git fetch ; [ $(git rev-parse HEAD) = $(git rev-parse @{u}) ]"
     else pure False
  where run (ExitFailure 1) = True
        run _               = False

--------------------
-- Shell Commands --
--------------------

checkDependency :: String -> IO ()
checkDependency app = which app >>= check
  where check Nothing = error $ "ERROR: Missing " ++ app ++ " on system path!"
        check _       = pure ()

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

-------------
-- Scripts --
-------------

mkCmdIn :: FilePath -> String -> String
mkCmdIn p c = [i|bash -c "pushd #{p} > /dev/null 2>&1 ; #{c} ; popd > /dev/null 2>&1"|]

runCmdIn :: FilePath -> String -> IO ExitCode
runCmdIn p c = system $ mkCmdIn p c

runScript :: Maybe FilePath -> IO ExitCode
runScript Nothing  = exitSuccess
runScript (Just s) = system $ "sudo sh " ++ s

----------------
-- Filesystem --
----------------

type CheckPath = FilePath -> FilePath
type CheckMaybePath = Maybe FilePath -> Maybe FilePath

checkPath :: FilePath -> FilePath -> FilePath -> FilePath
checkPath _ _ f@('/':_) = f
checkPath h _ ('~':rx)  = h ++ rx
checkPath _ d other     = d </> other

checkMaybePath :: FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath
checkMaybePath h d (Just p) = Just $ checkPath h d p
checkMaybePath _ _ _        = Nothing

removeFiles :: [FilePath] -> IO ()
removeFiles [] = pure ()
removeFiles xs = forM_ xs removeIfExists
  where removeIfExists f = do
          exists <- doesFileExist f
          if exists
             then removeFile f
             else pure ()
