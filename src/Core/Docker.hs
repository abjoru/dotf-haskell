{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Core.Docker where

import           Core.Os
import qualified Core.Term               as Term
import           Core.Types

import           Data.String.Interpolate (i)
import           Data.Yaml

import           System.Directory
import           System.FilePath         (isExtensionOf, (</>))

genCompose :: Config -> IO ()
genCompose cfg = do
  composePath <- getXdgDirectory XdgConfig "compose"
  outputPath  <- getXdgDirectory XdgCache "compose"
  composeData <- mkDockerComposeFile
  envData     <- mkDockerEnvFile $ configDocker cfg

  -- remove old stuff
  Term.info [i|Checking output path #{outputPath}...|]
  createDirectoryIfMissing True outputPath
  removeFiles [outputPath </> "docker-compose.yml", outputPath </> ".env"]

  -- write new stuff
  Term.info "Assembling new compose and env files..."
  writeFile (outputPath </> ".env") envData
  encodeFile (outputPath </> "docker-compose.yml") composeData

genOpenVPN :: Config -> IO ()
genOpenVPN cfg = do
  targetDir <- getXdgDirectory XdgCache "openvpn"
  targetExist <- doesFileExist $ targetDir </> "default.ovpn"

  if targetExist
     then Term.info [i|OpenVPN config already exist at #{targetDir </> "default.ovpn"}!|]
     else mkOpenVPNFile $ configDocker cfg
