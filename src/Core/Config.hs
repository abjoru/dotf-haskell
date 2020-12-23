{-# LANGUAGE OverloadedStrings #-}
module Core.Config (mkEnvFile) where

import Core.Types
import Core.Format

import Data.Maybe (fromMaybe)
import Data.List (isInfixOf, find)
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T

import System.Directory
import System.FilePath ((</>), isExtensionOf, takeFileName)

mkEnvFile :: Config -> IO String 
mkEnvFile cfg = do
  envMap    <- internalLoadMaps
  overrides <- loadSecrets $ configEnvOverride cfg
  return $ mkConfigStr $ HML.union overrides $ HML.union (mkSecrets cfg) envMap

mkSecrets :: Config -> HML.HashMap String String
mkSecrets cfg = 
  HML.unions [ HML.fromList $ netm $ configNetwork cfg
             , HML.fromList $ vpnm $ configVpn cfg
             , HML.fromList $ sysm $ configSystem cfg
             , HML.fromList $ medm $ configMedia cfg
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

internalLoadMaps :: IO (HML.HashMap String String)
internalLoadMaps = do
  dir <- getXdgDirectory XdgConfig "compose"
  files <- listDirectory dir
  maps <- mapM (parseWith dir) $ filter ("env" `isExtensionOf`) files
  return $ HML.unions maps
    where parseWith d f = readEnvFile $ d </> f

loadSecrets :: Maybe FilePath -> IO (HML.HashMap String String) 
loadSecrets (Just f) = readEnvFile f
loadSecrets Nothing  = pure HML.empty 

readEnvFile :: FilePath -> IO (HML.HashMap String String)
readEnvFile p = do
  content <- readFile p
  return $ HML.fromList $ map split (filter f $ lines content)
    where split :: String -> (String, String)
          split x = case T.splitOn "=" $ T.pack x of
                      (k:v:_) -> (T.unpack k, T.unpack v)
                      (k:_) -> (T.unpack k, T.unpack k)

          f ('#':_) = False
          f other   = "=" `isInfixOf` other

findInDir :: (FilePath -> Bool) -> FilePath -> IO (Maybe FilePath)
findInDir f d = do
  ls <- listDirectory d
  return $ find f ls
