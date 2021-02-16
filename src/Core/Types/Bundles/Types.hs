module Core.Types.Bundles.Types where

type Name = String
type IsAur = Bool
type IsCask = Bool
type IsHead = Bool
type Script = FilePath

data PkgSystem = Pacman | Homebrew | Apt
  deriving Show

newtype OsPackages = OsPackages [String]
  deriving Show

data BundleConfig = BundleConfig
  { name :: String
  , description :: String
  , bundles :: [Bundle]
  } deriving Show

data Bundle = Bundle
  { bundleName :: Name
  , bundleHeadless :: Bool
  , bundlePackages :: [Package]
  , bundleGitPkgs :: [Git]
  , bundleScript :: Maybe Script
  , bundlePreInstall :: Maybe Script
  , bundlePostInstall :: Maybe Script
  } deriving Show

data Package = Pac Name IsAur
             | Brew Name IsCask IsHead
             | Deb Name
             deriving Show

data Git = Git
  { gitName :: Name
  , gitUrl :: String
  , gitBranch :: Maybe String
  , gitSubmodules :: Bool
  , gitInstallScript :: Maybe FilePath 
  , gitInstallCommand :: Maybe String
  , gitTargetPath :: Maybe FilePath
  } deriving Show
