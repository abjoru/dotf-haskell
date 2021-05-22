{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Workflow.Compose (
  composeUp,
  composeDown,
  composePull,
  composeRestart,
  composeShow
) where

import           GHC.Exts                (fromList)

import           Core.Format
import           Core.Options
import           Core.Os
import qualified Core.Term               as Term
import           Core.Types
import           Core.Utils

import qualified Data.HashMap.Lazy       as HML
import           Data.String.Interpolate (i)
import           Data.Text               (Text (..), unpack)
import           Data.Yaml

import           System.Directory
import           System.FilePath         (isExtensionOf, (</>))
import           System.Process

composeShow :: IO ()
composeShow = do
  services <- serviceNames
  mapM_ Term.info services

-- Creating containers for input service names or all
composeUp :: DryMode -> [String] -> IO ()
composeUp dm [] = do
  sn <- serviceNames
  if not $ null sn
     then execCompose dm $ mkString "up -d --remove-orphans " " " "" sn
     else Term.info "No services to start!"
composeUp dm services = execCompose dm $ mkString "up -d --remove-orphans " " " "" services

-- Stopping and removing containers, networks, volumes, and images.
composeDown :: DryMode -> IO ()
composeDown dm = execCompose dm "down --remove-orphans"

-- Pulling the latest images for 'serviceName' or all services
composePull :: DryMode -> [String] -> IO ()
composePull dm [] = do
  sn <- serviceNames
  if not $ null sn
     then execCompose dm $ mkString "pull --include-deps " " " "" sn
     else Term.info "No services to pull!"
composePull dm services = execCompose dm $ mkString "pull --include-deps " " " "" services

-- Restarting 'serviceName' or all stopped and running services
composeRestart :: DryMode -> [String] -> IO ()
composeRestart dm [] = do
  sn <- serviceNames
  if not $ null sn
     then execCompose dm $ mkString "restart " " " "" sn
     else Term.info "No services to restart"
composeRestart dm services = execCompose dm $ mkString "restart " " " "" services

execCompose :: DryMode -> String -> IO ()
execCompose dm cmd = do
  maybeExe <- which "docker-compose"
  composeP <- getXdgDirectory XdgCache "compose"
  case (dm, maybeExe) of
    (Dry, Just exe) -> putStrLn [i|#{exe} --project-directory #{composeP} #{cmd}|]
    (Normal, Just exe) -> system [i|#{exe} --project-directory #{composeP} #{cmd}|] >> pure ()
    (_, Nothing)  -> Term.err "Unable to find docker-compose. Please make sure it's intalled!"

serviceNames :: IO [String]
serviceNames = do
  composeData <- mkDockerComposeFile
  case composeData of
    Object o -> return $ readKeys (HML.lookup "services" o)
    _        -> return []
  where readKeys (Just (Object o)) = map unpack $ HML.keys o
        readKeys _                 = []
