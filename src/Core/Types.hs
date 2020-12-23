{-# LANGUAGE OverloadedStrings #-} 
module Core.Types (
  module Core.Types.Types,
  module Core.Types.Instances,

  -- decoders
  decodeConfig,
  decodeBundles,
  decodeGroups
) where

import Core.Types.Types
import Core.Types.Instances
import Core.Types.Parsers



import Control.Arrow (left)

import Data.Yaml
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import qualified Data.Aeson.Extra.Merge as M

import System.Directory
import System.FilePath ((</>))

--------------
-- Decoders --
--------------

type CheckPath = FilePath -> FilePath 
type CheckMaybePath = Maybe FilePath -> Maybe FilePath 

-- Normalize path
internalCheckPath :: FilePath -> FilePath -> FilePath -> FilePath 
internalCheckPath _ _ f@('/': _)  = f
internalCheckPath h _ ('~': rx)  = h ++ rx
internalCheckPath _ d other      = d </> other

-- Normalize maybe path
internalCheckPath' :: FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath
internalCheckPath' h d (Just p) = Just $ internalCheckPath h d p
internalCheckPath' _ _ _        = Nothing 

-- Check paths in VPN instance
internalCheckVpn :: CheckPath -> CheckMaybePath -> Maybe Vpn -> Maybe Vpn
internalCheckVpn cp cp' (Just v) = Just v { vpnConfigDir = cp $ vpnConfigDir v
                                          , vpnWireguardDir = cp' $ vpnWireguardDir v
                                          }
internalCheckVpn _ _ other = other

-- Check system paths
internalCheckSystem :: CheckPath -> Maybe System -> Maybe System
internalCheckSystem cp (Just s) = Just $ s { systemDownloadDir = cp $ systemDownloadDir s
                                           , systemDockerConfigDir = cp $ systemDockerConfigDir s
                                           , systemDockerStorageDir = cp $ systemDockerStorageDir s
                                           , systemDockerSharedDir = cp $ systemDockerSharedDir s
                                           , systemDockerBackupDir = cp $ systemDockerBackupDir s
                                           }
internalCheckSystem _ other = other

-- Check media paths
internalCheckMedia :: CheckPath -> Maybe Media -> Maybe Media
internalCheckMedia cp (Just m) = Just $ m { mediaTvDir = cp $ mediaTvDir m
                                          , mediaBooksDir = cp $ mediaBooksDir m
                                          , mediaMusicDir = cp $ mediaMusicDir m
                                          , mediaMoviesDir = cp $ mediaMoviesDir m
                                          , mediaComicsDir = cp $ mediaComicsDir m
                                          , mediaAudiobooksDir = cp $ mediaAudiobooksDir m
                                          }
internalCheckMedia _ other = other

-- Decode `Config` instance
decodeConfig :: FilePath -> IO (Either ParseException Config)
decodeConfig f = do
  home <- getHomeDirectory 
  dotf <- getXdgDirectory XdgConfig "dotf"
  dcfg <- decodeFileEither f

  let checkPath = internalCheckPath home dotf
      checkPath' = internalCheckPath' home dotf

  return $ case dcfg of
    (Right c) -> Right $ c { configGitDirectory = checkPath $ configGitDirectory c
                           , configHomeDirectory = home
                           , configDirectory = dotf
                           , configRepoDirectory = checkPath $ configRepoDirectory c
                           , configVpn = internalCheckVpn checkPath checkPath' $ configVpn c
                           , configSystem = internalCheckSystem checkPath $ configSystem c
                           , configMedia = internalCheckMedia checkPath $ configMedia c
                           }
    other -> other

-- Decode package overrides
decodeOverride :: FromJSON a => FilePath -> Maybe FilePath -> IO (Either String a)
decodeOverride mf (Just ef) = do
  mv <- decodeFileEither mf :: IO (Either ParseException Value)
  ev <- decodeFileEither ef :: IO (Either ParseException Value)
  return $ parseBoth mv ev
    where parseBoth :: FromJSON a => Either ParseException Value -> Either ParseException Value -> Either String a
          parseBoth (Right a) (Right b) = parseEither parseJSON $ merge a b
          parseBoth (Right a) _         = parseEither parseJSON a
          parseBoth (Left er) _         = Left $ prettyPrintParseException er
decodeOverride mf Nothing = do
  mv <- decodeFileEither mf
  return $ left prettyPrintParseException mv

-- Aeson Value merge (left-bias)
merge :: Value -> Value -> Value
merge = M.merge f
  where f r (M.ObjectF a) (M.ObjectF b) = M.ObjectF $ HM.unionWith r a b
        f _ (M.ArrayF a)  (M.ArrayF b)  = M.ArrayF $ a <> b
        f _ a _                         = a

-- Decode `InstallConfig` instance
decodeBundles :: Config -> FilePath -> IO (Either String InstallConfig)
decodeBundles c f = do
  cfg <- decodeOverride f (configExtensionFile c)
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
          checkDir (Just ('~':r)) c   = Just (configHomeDirectory c ++ r)
          checkDir (Just r) c         = Just (configDirectory c </> r)
          checkDir o _                = o

-- Decode `Groups` instance
decodeGroups :: FilePath -> IO (Either ParseException Groups)
decodeGroups = decodeFileEither
