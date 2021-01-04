{-# LANGUAGE DeriveGeneric #-} 
module Core.Types.Types where

import GHC.Generics

data PkgSystem = Pacman | Homebrew | Apt
  deriving Show

data Env = Env
  { pkgSystem :: PkgSystem
  , config :: Config
  , install :: InstallConfig
  } deriving Show

data Config = Config
  { configHeadless :: Bool 
  , configGitDirectory :: FilePath 
  , configHomeDirectory :: FilePath 
  , configDirectory :: FilePath 
  , configRepoDirectory :: FilePath 
  , configExtensionFile :: Maybe FilePath 
  , configHomepage :: Maybe Homepage
  , configNetwork :: Maybe Network
  , configVpn :: Maybe Vpn
  , configSystem :: Maybe System
  , configMedia :: Maybe Media
  , configEnvOverride :: Maybe FilePath
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

data Homepage = Homepage
  { homepageHeader :: FilePath
  , homepageFooter :: FilePath
  , homepageLinks :: FilePath
  , homepageCss :: FilePath
  } deriving Show

newtype Groups = Groups [Group]
  deriving Show

data Group = Group
  { groupName :: String
  , groupFilter :: Maybe String
  , groupLinks :: [Link]
  } deriving Show

data Link = Link
  { linkId :: String
  , linkName :: String
  , linkUrl :: String
  } deriving Show

data OsPkgs = OsPkgs
  { systemPkgs :: [String]
  , pipPkgs :: [String]
  } deriving Show

newtype PipList = PipList [PipPkg]
  deriving (Show, Generic)

data PipPkg = PipPkg
  { pipName :: String
  , pipVersion :: String
  } deriving Show

data InstallConfig = InstallConfig
  { name :: String
  , description :: String
  , bundles :: [Bundle]
  } deriving Show

data Bundle = Bundle
  { bundleName :: String
  , bundleHeadless :: Bool
  , bundlePackages :: [Pkg]
  , bundleGitPkgs :: [GitPkg]
  , bundlePipPkgs :: [String]
  , bundleScript :: Maybe FilePath
  , bundlePreInstall :: Maybe FilePath
  , bundlePostInstall :: Maybe FilePath
  } deriving Show

data Pkg = Pkg
  { pkgName :: String
  , pkgAur :: Bool
  , pkgCask :: Bool
  } deriving Show

data GitPkg = GitPkg
  { gitName :: String
  , gitUrl :: String
  , gitBranch :: Maybe String
  , gitSubmodules :: Bool
  , gitInstallScript :: Maybe FilePath
  , gitInstallCommand :: Maybe String
  , gitTargetPath :: Maybe FilePath
  } deriving Show
