module Core.Types.Docker.Types where

data DockerConfig = DockerConfig
  { dockerNetwork :: Maybe Network
  , dockerVpn :: Maybe Vpn
  , dockerSystem :: Maybe System
  , dockerMedia :: Maybe Media
  , dockerEnvOverride :: Maybe FilePath
  } deriving Show

data Network = Network
  { networkHostname :: String 
  , networkLan :: String 
  , networkNs1 :: String 
  , networkNs2 :: String 
  } deriving Show

data Vpn = Vpn
  { vpnClient :: String
  , vpnOptions :: String
  , vpnConfigDir :: FilePath
  , vpnPassword :: String
  , vpnUsername :: String
  , vpnProvider :: String
  , vpnConfig :: Maybe String
  , vpnWireguardDir :: Maybe FilePath
  } deriving Show

data System = System
  { systemDownloadDir :: FilePath
  , systemDockerConfigDir :: FilePath
  , systemDockerStorageDir :: FilePath
  , systemDockerSharedDir :: FilePath
  , systemDockerBackupDir :: FilePath
  } deriving Show

data Media = Media
  { mediaTvDir :: FilePath
  , mediaBooksDir :: FilePath
  , mediaMusicDir :: FilePath
  , mediaMoviesDir :: FilePath
  , mediaComicsDir :: FilePath
  , mediaAudiobooksDir :: FilePath
  } deriving Show
