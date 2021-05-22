{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Core.Types.Docker (
  module Core.Types.Docker.Types,

  composeVersion,
  mkOpenVPNFile,
  mkDockerEnvFile,
  mkDockerComposeFile
) where

import           GHC.Exts                (fromList)

import           Control.Monad           (when)

import           Core.Term               as Term
import           Core.Types.Docker.Types

import           Core.Format

import qualified Data.HashMap.Lazy       as HML
import           Data.List               (find, isInfixOf)
import           Data.Maybe              (fromMaybe)
import           Data.String.Interpolate (i)
import           Data.Text               (Text (..), unpack)
import qualified Data.Text               as T
import           Data.Yaml

import           Codec.Archive.Zip
import           Network.HTTP.Simple

import           System.Directory
import           System.FilePath         (isExtensionOf, takeFileName, (</>))

composeVersion :: Text
composeVersion = "3.8"

mkDockerEnvFile :: Maybe DockerConfig -> IO String
mkDockerEnvFile Nothing    = pure ""
mkDockerEnvFile (Just cfg) = do
  envMap <- loadMaps
  overrides <- loadSecrets $ dockerEnvOverride cfg
  return $ mkConfigStr $ HML.union overrides $ HML.union (mkSecrets cfg) envMap

mkDockerComposeFile :: IO Value
mkDockerComposeFile = do
  servicePath <- getXdgDirectory XdgConfig "compose"
  services    <- readServices servicePath
  return $ Object $ fromList [ ("version", String composeVersion)
                             , ("services", flatten (foldl fo [] services))
                             ]
  where fo acc (Right (Object o)) = acc ++ [o]
        fo _ (Right _)            = fail "Expected Object for yml file..."
        fo _ (Left er)            = fail $ show er

        flatten = Object . HML.unions

        readServices p = do
          files <- listDirectory p
          mapM (decodeFileEither . (p </>)) (filter ("yaml" `isExtensionOf`) files)

mkOpenVPNFile :: Maybe DockerConfig -> IO ()
mkOpenVPNFile Nothing = return ()
mkOpenVPNFile (Just cfg) = do
  targetDir <- getXdgDirectory XdgCache "openvpn"
  targetExist <- doesFileExist $ targetDir </> "default.ovpn"

  if targetExist
     then Term.info [i|OpenVPN config already exist at #{targetDir </> "default.ovpn"}!|]
     else fetchPia cfg targetDir

-----------
-- Utils --
-----------

fetchPia :: DockerConfig -> FilePath -> IO ()
fetchPia cfg fp = do
  createDirectoryIfMissing True fp
  downloadPiaConfigs fp
  createDefaultOvpn cfg fp

  case dockerVpn cfg of
    Just vpn -> createPasswordFile vpn fp
    Nothing  -> return ()

downloadPiaConfigs :: FilePath -> IO ()
downloadPiaConfigs fp = do
  response <- httpLBS "https://www.privateinternetaccess.com/openvpn/openvpn.zip"
  Term.info "Downloading PIA configs from https://www.privateinternetaccess.com/openvpn/openvpn.zip"
  let archive = toArchive $ getResponseBody response

  extractFilesFromArchive [OptDestination fp] archive

createDefaultOvpn :: DockerConfig -> FilePath -> IO ()
createDefaultOvpn cfg fp = do
  remFile $ fp </> "default.ovpn"
  contents <- readFile $ fp </> baseCfg cfg
  Term.info [i|Writing default.ovpn from #{baseCfg cfg}...|]
  writeFile (fp </> "default.ovpn") $ unlines . map f $ lines contents
    where f line | "auth-user-pass" `isInfixOf` line = "auth-user-pass /config/pia-creds.txt"
                 | otherwise                         = line

          baseCfg :: DockerConfig -> String
          baseCfg c = fromMaybe "us_florida.ovpn" $ vpnConfig =<< dockerVpn c

          remFile f = do
            exists <- doesFileExist f
            when exists $ removeFile f

createPasswordFile :: Vpn -> FilePath -> IO ()
createPasswordFile vpn fp =
  let file = fp </> "pia-creds.txt"
      contents = [vpnUsername vpn, vpnPassword vpn]
   in write file $ unlines contents
  where write f c = Term.info [i|Writing PIA credentials to #{f}...|] >> writeFile f c

mkSecrets :: DockerConfig -> HML.HashMap String String
mkSecrets cfg = HML.unions [ HML.fromList $ netm $ dockerNetwork cfg
                           , HML.fromList $ vpnm $ dockerVpn cfg
                           , HML.fromList $ sysm $ dockerSystem cfg
                           , HML.fromList $ medm $ dockerMedia cfg
                           ]
  where netm (Just n) = [ ("DOCKER_HOSTNAME", networkHostname n)
                        , ("LAN_NETWORK", networkLan n)
                        , ("NS1", networkNs1 n)
                        , ("NS2", networkNs2 n)
                        ]
        netm _ = []

        vpnm (Just v) = [ ("VPN_CLIENT", vpnClient v)
                        , ("VPN_OPTIONS", vpnOptions v)
                        , ("VPN_OVPNDIR", vpnConfigDir v)
                        , ("VPN_PASS", vpnPassword v)
                        , ("VPN_USER", vpnUsername v)
                        , ("VPN_PROV", vpnProvider v)
                        , ("VPN_CONFIG", fromMaybe "" $ vpnConfig v)
                        , ("VPN_WGDIR", fromMaybe "" $ vpnWireguardDir v)
                        ]
        vpnm _ = []

        sysm (Just s) = [ ("DOWNLOAD_DIR", systemDownloadDir s)
                        , ("DOCKER_CONF_DIR", systemDockerConfigDir s)
                        , ("DOCKER_STORAGE_DIR", systemDockerStorageDir s)
                        , ("DOCKER_SHARED_DIR", systemDockerSharedDir s)
                        , ("BACKUP_CONF_DIR", systemDockerBackupDir s)
                        ]
        sysm _ = []

        medm (Just m) = [ ("MEDIA_TV", mediaTvDir m)
                        , ("MEDIA_BOOKS", mediaBooksDir m)
                        , ("MEDIA_MUSIC", mediaMusicDir m)
                        , ("MEDIA_MOVIES", mediaMoviesDir m)
                        , ("MEDIA_COMICS", mediaComicsDir m)
                        , ("MEDIA_AUDIOBOOKS", mediaAudiobooksDir m)
                        ]
        medm _ = []

loadMaps :: IO (HML.HashMap String String)
loadMaps = do
  dir <- getXdgDirectory XdgConfig "compose"
  files <- listDirectory dir
  maps <- mapM (parseWith dir) $ filter ("env" `isExtensionOf`) files
  return $ HML.unions maps
    where parseWith d f = readEnvFile $ d </> f

loadSecrets :: Maybe FilePath -> IO (HML.HashMap String String)
loadSecrets (Just f) = readEnvFile f
loadSecrets Nothing  = pure HML.empty

readEnvFile :: FilePath -> IO (HML.HashMap String String)
readEnvFile fp = do
  content <- readFile fp
  return $ HML.fromList $ map split (filter f $ lines content)
    where split x = case T.splitOn "=" $ T.pack x of
                      (k:v:_) -> (T.unpack k, T.unpack v)
                      (k:_)   -> (T.unpack k, T.unpack k)
          f ('#':_) = False
          f other   = "=" `isInfixOf` other
