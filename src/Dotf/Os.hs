{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Dotf.Os where

import Data.Text (Text)
import Data.Yaml

import System.Info
import System.Exit
import System.Process
import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath

import Control.Applicative

data Config = Config 
  { headless :: Bool
  , pkgSystem :: String
  } deriving (Show, Eq)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config 
    <$> o .: "headless"
    <*> o .: "package-system"

-- Load app config or die!
loadCfg :: IO Config
loadCfg = do
  dd <- getXdgDirectory XdgConfig "dotf"
  de <- decodeFileEither $ dd </> "dotf.yaml"
  case de of
    Right c -> return c
    Left (InvalidYaml (Just (YamlException e))) -> error $ "Could not read config file: " ++ e
    Left er -> error $ "Could not read config file: " ++ show er

--------------------
-- Shell Commands --
--------------------

-- Check if prog exists on system
which :: String -> IO (Maybe String)
which arg = do
  ph <- spawnProcess "which" [arg]
  ec <- waitForProcess ph
  case ec of
    ExitSuccess   -> pure $ Just arg
    ExitFailure _ -> pure Nothing
