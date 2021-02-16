{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Decoders (
  decodeConfig,
  decodeBundleConfig,
  decodeHomepageGroups
) where

import Core.Types.Types
import Core.Types.Instances
import Core.Types.Bundles
import Core.Types.Bundles.Parsers
import Core.Types.Docker
import Core.Types.Docker.Decoders
import Core.Types.Startpage

import Core.Os
import Core.Utils

import Data.Yaml
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Extra.Merge as M

import System.Directory
import System.FilePath ((</>))

decodeConfig :: FilePath -> IO (Either ParseException Config)
decodeConfig fp = do
  home <- getHomeDirectory
  dotf <- getXdgDirectory XdgConfig "dotf"
  dcfg <- decodeFileEither fp
  mdoc <- checkDockerConfig $ extractDockerConfig dcfg

  let checkP  = checkPath home dotf
      checkMP = checkMaybePath home dotf

  return $ case dcfg of
    (Right c) -> Right $ c { configGitDirectory = checkP $ configGitDirectory c
                           , configHomeDirectory = home
                           , configDirectory = dotf
                           , configRepoDirectory = checkP $ configRepoDirectory c 
                           , configDocker = mdoc
                           }
    other -> other
    where extractDockerConfig (Right c) = configDocker c
          extractDockerConfig _         = Nothing

decodeBundleConfig :: PkgSystem -> Config -> FilePath -> IO (Either String BundleConfig)
decodeBundleConfig sys cfg p = do
  icfg <- decodeFileEither p :: IO (Either ParseException Value)
  ecfg <- decodeMaybeFile (configExtensionFile cfg) :: IO (Either ParseException Value)
  return $ parseBoth icfg ecfg 
    where parseBoth :: Either ParseException Value -> Either ParseException Value -> Either String BundleConfig
          parseBoth (Right a) (Right b) = parseEither (parseBundleConfig sys) $ merge a b
          parseBoth (Right a) _         = parseEither (parseBundleConfig sys) a
          parseBoth (Left er) _         = Left $ prettyPrintParseException er

          decodeMaybeFile :: Maybe FilePath -> IO (Either ParseException Value)
          decodeMaybeFile (Just fp) = decodeFileEither fp
          decodeMaybeFile Nothing = pure $ Left NonScalarKey -- some arbitrary exception..

decodeHomepageGroups :: FilePath -> IO (Either ParseException Groups)
decodeHomepageGroups = decodeFileEither
