{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Decoders (
  decodeConfig,
  decodeBundleConfig,
  decodeHomepageGroups
) where

import           Core.Types.Bundles
import           Core.Types.Bundles.Parsers
import           Core.Types.Docker
import           Core.Types.Docker.Decoders
import           Core.Types.Instances
import           Core.Types.Startpage
import           Core.Types.Types

import           Core.Os
import           Core.Utils

import qualified Data.Aeson.Extra.Merge     as M
import qualified Data.HashMap.Strict        as HM
import           Data.Yaml

import           System.Directory
import           System.FilePath            ((</>))

decodeConfig :: FilePath -> IO (Either ParseException Config)
decodeConfig fp = do
  home <- getHomeDirectory
  dotf <- getXdgDirectory XdgConfig "dotf"
  dcfg <- decodeFileEither fp

  let checkP  = checkPath home dotf
      checkMP = checkMaybePath home dotf

  return $ case dcfg of
    (Right c) -> Right $ c { configGitDirectory = checkP $ configGitDirectory c
                           , configHomeDirectory = home
                           , configDirectory = dotf
                           , configRepoDirectory = checkP $ configRepoDirectory c
                           , configDocker = fixDockerConfig checkP checkMP $ configDocker c
                           , configHomepage = fixPaths checkP $ configHomepage c
                           }
    other -> other
    where fixPaths _ Nothing   = Nothing
          fixPaths cp (Just h) = Just $ h { homepageHeader = cp $ homepageHeader h
                                       , homepageFooter = cp $ homepageFooter h
                                       , homepageLinks  = cp $ homepageLinks h
                                       , homepageStylesheet = cp $ homepageStylesheet h
                                       }

decodeBundleConfig :: PkgSystem -> Config -> FilePath -> IO (Either String BundleConfig)
decodeBundleConfig sys cfg p = do
  icfg <- decodeFileEither p :: IO (Either ParseException Value)
  ecfg <- decodeMaybeFile (configExtensionFile cfg) :: IO (Either ParseException Value)

  return $ parseBoth icfg ecfg
    where parseBoth :: Either ParseException Value -> Either ParseException Value -> Either String BundleConfig
          parseBoth (Right a) (Right b) = parseEither (parseBundleConfig sys cmp) $ merge a b
          parseBoth (Right a) _         = parseEither (parseBundleConfig sys cmp) a
          parseBoth (Left er) _         = Left $ prettyPrintParseException er

          cmp = checkMaybePath (configHomeDirectory cfg) (configDirectory cfg)

          decodeMaybeFile :: Maybe FilePath -> IO (Either ParseException Value)
          decodeMaybeFile (Just fp) = decodeFileEither fp
          decodeMaybeFile Nothing   = pure $ Left NonScalarKey -- some arbitrary exception..

decodeHomepageGroups :: FilePath -> IO (Either ParseException Groups)
decodeHomepageGroups = decodeFileEither
