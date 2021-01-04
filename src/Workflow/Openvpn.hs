{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Workflow.Openvpn (genVpnConfig) where
  
import Control.Monad (when)

import Core.Types
import qualified Core.Term as Term

import Data.List (isInfixOf)
import Data.String.Interpolate (i)

import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Simple

import Codec.Archive.Zip

import System.Directory
import System.FilePath ((</>))

genVpnConfig :: Env -> IO ()
genVpnConfig env = do
  targetDir <- getXdgDirectory XdgCache "openvpn"
  targetExist <- doesFileExist $ targetDir </> "default.ovpn"

  if targetExist
     then Term.info [i|OpenVPN config already exists at #{targetDir </> "default.ovpn"}!|]
     else internalGenerateVPNFiles (config env) targetDir

internalGenerateVPNFiles :: Config -> FilePath -> IO ()
internalGenerateVPNFiles cfg tp = do
  createDirectoryIfMissing True tp
  downloadPiaConfigs tp
  createDefaultOvpn tp

  case configVpn cfg of
    Just vpn -> createPasswordFile vpn tp
    Nothing  -> return ()

-- Download and extract OpenVPN configs to target directory
downloadPiaConfigs :: FilePath -> IO ()
downloadPiaConfigs targetDir = do
  response <- httpLBS "https://www.privateinternetaccess.com/openvpn/openvpn.zip"
  Term.info "Downloading PIA configs from: https://www.privateinternetaccess.com/openvpn/openvpn.zip"
  let archive = toArchive $ getResponseBody response
  extractFilesFromArchive [OptDestination targetDir] archive

-- Replace 'default.ovpn' with our own version that matches credentials
-- from the given file-path (in terms of docker container mapped path)
-- TODO make base file configurable (i.e. the us_florida.ovpn one)
createDefaultOvpn :: FilePath -> IO ()
createDefaultOvpn targetDir = do
  remFile $ targetDir </> "default.ovpn"
  contents <- readFile $ targetDir </> "us_florida.ovpn"
  Term.info "Writing default.ovpn from us_florida.ovpn openvpn config..."
  writeFile (targetDir </> "default.ovpn") $ unlines . map f $ lines contents
    where f line | "auth-user-pass" `isInfixOf` line = "auth-user-pass /config/pia-creds.txt"
                 | otherwise                         = line

-- Create password file for PIA
createPasswordFile :: Vpn -> FilePath -> IO ()
createPasswordFile vpn td = 
  let file     = td </> "pia-creds.txt"
      contents = [vpnUsername vpn, vpnPassword vpn]
   in write file $ unlines contents
  where write f c = Term.info [i|Writing PIA credentials to #{f}...|] >> writeFile f c

-- Safe(er) remove file function. Will check if file exist first
remFile :: FilePath -> IO ()
remFile f = do
  exists <- doesFileExist f
  when exists $ removeFile f
