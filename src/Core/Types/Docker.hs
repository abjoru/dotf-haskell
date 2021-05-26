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

import           Control.Monad           (filterM, forM_, when)

import           Core.Os
import           Core.Term               as Term
import           Core.Types.Docker.Types

import           Core.Format

import qualified Data.HashMap.Lazy       as HML
import           Data.List               (find, isInfixOf, isPrefixOf)
import           Data.Maybe              (fromMaybe)
import           Data.String.Interpolate (i)
import           Data.Text               (Text (..), unpack)
import qualified Data.Text               as T
import           Data.Yaml

import           Codec.Archive.Zip
import           Network.HTTP.Simple

import           System.Directory
import           System.FilePath         (isExtensionOf, takeBaseName, (</>))
import           System.Posix.User

composeVersion :: Text
composeVersion = "3.8"

mkDockerEnvFile :: UserEntry -> Maybe DockerConfig -> IO String
mkDockerEnvFile _ Nothing      = pure ""
mkDockerEnvFile usr (Just cfg) = do
  envMap <- fmap (addUser usr . addGroup usr) loadMaps
  overrides <- loadSecrets $ dockerEnvOverride cfg
  return $ mkConfigStr $ HML.union overrides $ HML.union (mkSecrets cfg) envMap
    where addUser u = HML.insert "PUID" $ show (userID u)
          addGroup u = HML.insert "PGID" $ show (userGroupID u)

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
mkOpenVPNFile Nothing = Term.warn "No docker config defined in dotf.yaml!" >> return ()
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
  createTransmissionOvpn (dockerVpn cfg) fp
  createNzbGetOvpnFolder fp

  --createDefaultOvpn (dockerVpn cfg) fp
  --ovpnFiles <- listOvpnFiles fp

  --let fOvpn = map toAbs $ filter ("default.ovpn" /=) ovpnFiles
   --in removeFiles fOvpn
  --where toAbs p = fp </> p

downloadPiaConfigs :: FilePath -> IO ()
downloadPiaConfigs fp = do
  response <- httpLBS "https://www.privateinternetaccess.com/openvpn/openvpn.zip"
  Term.info "Downloading PIA configs from https://www.privateinternetaccess.com/openvpn/openvpn.zip"
  let archive = toArchive $ getResponseBody response

  extractFilesFromArchive [OptDestination fp] archive

createTransmissionOvpn :: Maybe Vpn -> FilePath -> IO ()
createTransmissionOvpn Nothing _ = Term.warn "Missing docker config in dotf.yaml!"
createTransmissionOvpn (Just vpn) fp = do
  let sourceFile = fromMaybe "us_florida.ovpn" $ vpnConfig vpn
  contents <- readFile $ fp </> sourceFile
  Term.info [i|Writing transmission.ovpn from #{sourceFile}...|]
  writeFile (fp </> "transmission.ovpn") $ unlines . map f $ lines contents
    where f line | "auth-user-pass" `isInfixOf` line = "auth-user-pass /config/openvpn-credentials.txt"
            | otherwise                              = line

createNzbGetOvpnFolder :: FilePath -> IO ()
createNzbGetOvpnFolder fp = do
  createDirectoryIfMissing True $ fp </> "nzbget"
  allFiles <- listFiles fp
  Term.info [i|allFiles=#{allFiles}|]

  let ovpnFiles = filter (\x -> isPrefixOf "us_" $ takeBaseName x) allFiles
      otherFiles = filter (not . isExtensionOf "ovpn") allFiles
      all = ovpnFiles ++ otherFiles
   in forM_ all $ cpy fp (fp </> "nzbget")
  where cpy src dest f = copyFile (src </> f) (dest </> f)

createDefaultOvpn :: Maybe Vpn -> FilePath -> IO ()
createDefaultOvpn (Just vpn) fp = do
  renameFile (fp </> baseCfg vpn) (fp </> "default.ovpn")
    where baseCfg v = fromMaybe "us_florida.ovpn" $ vpnConfig v
createDefaultOvpn _ _ = Term.warn "Missing VPN configuration in dotf.yaml!" >> pure ()

listOvpnFiles :: FilePath -> IO [FilePath]
listOvpnFiles fp = do
  files <- listDirectory fp
  return $ filter ("ovpn" `isExtensionOf`) files

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
