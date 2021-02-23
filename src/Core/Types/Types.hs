module Core.Types.Types where

import           Core.Types.Bundles.Types
import           Core.Types.Docker.Types
import           Core.Types.Startpage.Types

data Env = Env
  { pkgSystem    :: PkgSystem
  , config       :: Config
  , bundleConfig :: BundleConfig
  } deriving Show

data Config = Config
  { configHeadless      :: Bool
  , configGitDirectory  :: FilePath
  , configHomeDirectory :: FilePath
  , configDirectory     :: FilePath
  , configRepoDirectory :: FilePath
  , configExtensionFile :: Maybe FilePath
  , configHomepage      :: Maybe Homepage
  , configDocker        :: Maybe DockerConfig
  } deriving Show
