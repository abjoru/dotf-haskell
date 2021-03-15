{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Workflow.Compose (
  composeUp,
  composeDown,
  composePull,
  composeRestart,
  composeShow,
  genCompose
) where

import GHC.Exts (fromList)

import Core.Os
import Core.Utils
import Core.Types
import Core.Options
import Core.Format
import qualified Core.Term as Term

import Data.Yaml
import Data.Text (Text(..), unpack)
import Data.String.Interpolate (i)
import qualified Data.HashMap.Lazy as HML

import System.Directory
import System.FilePath ((</>), isExtensionOf)
import System.Process

-- Compose yaml version
composeVersion :: Text
composeVersion = "3.8"

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

genCompose :: Config -> IO ()
genCompose cfg = do
  composePath <- getXdgDirectory XdgConfig "compose"
  outputPath  <- getXdgDirectory XdgCache "compose"
  composeData <- mkCompose composePath
  envData     <- mkDockerEnvFile $ configDocker cfg

  -- remove the old stuff
  -- TODO backup old configs to backup dir (or drop backup feature)
  Term.info [i|Checking output path #{outputPath}..|]
  createDirectoryIfMissing True outputPath
  removeFiles [outputPath </> "docker-compose.yml", outputPath </> ".env"]

  -- write the new stuff
  Term.info "Assembling new docker-compose and env files.."
  writeFile (outputPath </> ".env") envData
  encodeFile (outputPath </> "docker-compose.yml") composeData

mkCompose :: FilePath -> IO Value
mkCompose fp = do
  services <- decServices fp
  return $ Object $ fromList [ ("version", String composeVersion)
                             , ("services", flatten (foldl fo [] services))
                             ]
  where fo acc (Right (Object o)) = acc ++ [o]
        fo _ (Right _)            = fail "Expected Object for yaml file..."
        fo _ (Left ex)            = fail $ show ex

        flatten = Object . HML.unions

        decServices p = do
          files <- listDirectory p
          mapM (decodeFileEither . (p </>)) (filter ("yaml" `isExtensionOf`) files)

serviceNames :: IO [String]
serviceNames = do
  composePath <- getXdgDirectory XdgConfig "compose"
  composeData <- mkCompose composePath 
  case composeData of
    Object o -> return $ readKeys (HML.lookup "services" o)
    _ -> return []
  where readKeys (Just (Object o)) = map unpack $ HML.keys o
        readKeys _ = []
