{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Core.Types where

import GHC.Generics

import Data.Yaml
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import System.Directory
import System.FilePath ((</>))

data PkgSystem = Pacman | Homebrew | Apt

data Env = Env
  { pkgSystem :: PkgSystem
  , config :: Config
  , install :: InstallConfig
  }
 
data Config = Config 
  { headless :: Bool
  , gitDirectory :: FilePath    -- target directory for git pkg clones (default: ~/.local/share)
  , homeDirectory :: FilePath   -- user home directory
  , configDirectory :: FilePath -- data directory for DotF (default: ~/.config/dotf)
  , repoDirectory :: FilePath   -- git bare repo directory for dotfiles (default: ~/.dotf)
  , homepage :: Maybe Homepage  -- Optional homepage definition
  } deriving Show

data Homepage = Homepage
  { header :: FilePath
  , footer :: FilePath
  , links :: FilePath
  , css :: FilePath
  } deriving Show

data Groups = Groups [Group]
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
  }

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
  } deriving Show

data PipList = PipList [PipPkg]
  deriving (Show, Generic)

data PipPkg = PipPkg
  { pipName :: String
  , pipVersion :: String
  } deriving (Show, Generic)

---------------
-- Instances --
---------------

instance FromJSON Config where
  parseJSON (Object o) = Config
    <$> o .: "headless"
    <*> o .: "git-target-dir"
    <*> pure "~/"
    <*> pure "~/.config/dotf" -- static dir
    <*> o .: "dotf-repo-dir"
    <*> o .:? "homepage"
  parseJSON _ = fail "Expected Object for Config!"

instance FromJSON Homepage where
  parseJSON (Object o) = Homepage
    <$> o .: "header"
    <*> o .: "footer"
    <*> o .: "links"
    <*> o .: "css"
  parseJSON _ = fail "Expected Object for Homepage"

instance FromJSON InstallConfig where
  parseJSON (Object o) = case (icName, icDesc) of
    (Just a, Just b) -> InstallConfig <$> parseJSON a <*> parseJSON b <*> bundles
    _                -> fail "Unable to parse config!"
    where icName  = HM.lookup "name" o
          icDesc  = HM.lookup "description" o
          bundles = parseBundles o

instance FromJSON Groups where
  parseJSON (Array xs) = Groups <$> mapM parseGroup (V.toList xs)
  parseJSON _          = fail "Expected Object for Links"

instance FromJSON PipList

instance FromJSON PipPkg

--------------------
-- Custom Parsers --
--------------------

parseBundles :: HM.HashMap T.Text Value -> Parser [Bundle]
parseBundles m = mapM pBundle items
  where items = filter bundlesOnly $ HM.toList m
        bundlesOnly (k, _) = k /= "name" && k /= "description"

        pBundle :: (T.Text, Value) -> Parser Bundle
        pBundle (k, Object o) =
          let name = T.unpack k
              pkgs = parsePkgs $ HM.lookup "packages" o
              gits = parseGitPkgs $ HM.lookup "git" o
              pipp = parsePipPkgs $ HM.lookup "pip" o
              hles = parseJSON $ HM.lookupDefault (Bool False) "headless" o
              scrp = parseJSON $ HM.lookupDefault Null "script" o
              pre  = parseJSON $ HM.lookupDefault Null "pre-install" o
              post = parseJSON $ HM.lookupDefault Null "post-install" o
          in Bundle name <$> hles
                         <*> pkgs
                         <*> gits
                         <*> pipp
                         <*> scrp
                         <*> pre
                         <*> post

parsePkgs :: Maybe Value -> Parser [Pkg]
parsePkgs (Just (Array a)) = mapM pkg $ V.toList a
  where pkg (String s) = return $ Pkg (T.unpack s) False False
        pkg (Object o) = 
          let name = HM.foldlWithKey' f "<unknown>" o
              aur  = parseJSON $ HM.lookupDefault (Bool False) "aur" o
              cask = parseJSON $ HM.lookupDefault (Bool False) "cask" o
          in Pkg name <$> aur <*> cask

        f :: String -> T.Text -> Value -> String
        f _ k Null = T.unpack k
        f a _ _    = a
parsePkgs _ = return []

parseGitPkgs :: Maybe Value -> Parser [GitPkg]
parseGitPkgs (Just (Array a)) = mapM pkg $ V.toList a
  where pkg (Object o) =
          let name   = HM.foldlWithKey' f "<unknown>" o
              url    = parseJSON $ HM.lookupDefault "<missing>" "url" o
              branch = parseJSON $ HM.lookupDefault Null "branch" o
              submod = parseJSON $ HM.lookupDefault (Bool False) "submodules" o
              script = parseJSON $ HM.lookupDefault Null "install" o
              cmd    = parseJSON $ HM.lookupDefault Null "command" o
          in GitPkg name <$> url <*> branch <*> submod <*> script <*> cmd

        f :: String -> T.Text -> Value -> String
        f _ k Null = T.unpack k
        f a _ _    = a
parseGitPkgs _ = return []

parsePipPkgs :: Maybe Value -> Parser [String]
parsePipPkgs (Just a@(Array _)) = parseJSON a
parsePipPkgs _ = return []

parseGroup :: Value -> Parser Group
parseGroup (Object o) = 
  let name = parseJSON $ HM.lookupDefault "<unknown>" "name" o
      filter = parseJSON $ HM.lookupDefault Null "host-filter" o
      links = parseLinks $ HM.lookup "links" o
   in Group <$> name <*> filter <*> links
parseGroup _ = fail "Expected Object for Group"

parseLinks :: Maybe Value -> Parser [Link]
parseLinks (Just (Array a)) = mapM link $ V.toList a
  where link :: Value -> Parser Link
        link (Object o) =
          let id   = HM.foldlWithKey' f "<unknown>" o
              name = parseJSON $ HM.lookupDefault "<unknown>" "name" o
              url  = parseJSON $ HM.lookupDefault "" "url" o
           in Link id <$> name <*> url

        f :: String -> T.Text -> Value -> String
        f _ k Null = T.unpack k
        f a _ _    = a

--------------
-- Decoders --
--------------

decodeConfig :: FilePath -> IO (Either ParseException Config)
decodeConfig f = do
  home <- getHomeDirectory
  adir <- getXdgDirectory XdgConfig "dotf"
  cfg  <- decodeFileEither f
  return $ checkConfig cfg home adir
    where checkConfig (Right c) h a = Right $ c { gitDirectory = checkDir h a $ gitDirectory c
                                                , homeDirectory = h
                                                , configDirectory = a
                                                , repoDirectory = checkDir h a $ repoDirectory c
                                                }
          checkConfig other _ _ = other

          checkDir :: FilePath -> FilePath -> FilePath -> FilePath
          checkDir _ _ f@('/':_)  = f
          checkDir h _ ('~':rest) = h ++ rest
          checkDir _ a other      = a </> other

decodeBundles :: Config -> FilePath -> IO (Either ParseException InstallConfig)
decodeBundles c f = do
  cfg <- decodeFileEither f
  return $ checkConfig cfg
    where checkConfig (Right v) = Right $ v { bundles = map checkBundle $ bundles v }
          checkConfig other = other

          checkBundle :: Bundle -> Bundle
          checkBundle b@(Bundle _ _ _ g _ f1 f2 f3) = 
            b { bundleScript = checkDir f1 c
              , bundlePreInstall = checkDir f2 c
              , bundlePostInstall = checkDir f3 c
              , bundleGitPkgs = map checkGitItem g
              }

          checkGitItem :: GitPkg -> GitPkg
          checkGitItem g@(GitPkg _ _ _ _ v _) = g { gitInstallScript = checkDir v c }

          checkDir (Just f@('/':_)) _ = Just f
          checkDir (Just ('~':r)) c   = Just (homeDirectory c ++ r)
          checkDir (Just r) c         = Just (configDirectory c </> r)

decodeGroups :: FilePath -> IO (Either ParseException Groups)
decodeGroups = decodeFileEither
