{-# LANGUAGE QuasiQuotes #-}
module Core.Types (
  module Core.Types.Types,
  module Core.Types.Decoders,
  module Core.Types.Bundles,
  module Core.Types.Startpage,
  module Core.Types.Docker,

  -- loaders
  loadDotfConfig,
  loadBundleConfig,

  -- Utils
  pkgName
) where

import           Core.Types.Bundles
import           Core.Types.Decoders
import           Core.Types.Docker
import           Core.Types.Instances
import           Core.Types.Startpage
import           Core.Types.Types

import           Data.String.Interpolate (i)

import           System.Directory
import           System.FilePath         ((</>))

---------
-- API --
---------

loadDotfConfig :: IO Config
loadDotfConfig = do
  cd <- getXdgDirectory XdgConfig "dotf"
  rs <- decodeConfig $ cd </> "dotf.yaml"

  case rs of
    Right x -> return x
    Left er -> error $ "Unable to load config: " ++ (cd </> "dotf.yaml") ++ " " ++ show er


loadBundleConfig :: PkgSystem -> Config -> IO BundleConfig
loadBundleConfig sys cfg = do
  rs <- decodeBundleConfig sys cfg $ configDirectory cfg </> installFilename sys

  case rs of
    Right x -> return x
    Left er -> do
      putStrLn [i|Error reading #{installFilename sys}!|]
      putStrLn er
      return $ BundleConfig "Empty" "" []
  where installFilename Pacman   = "pacman.yaml"
        installFilename Homebrew = "brew.yaml"
        installFilename Apt      = "apt.yaml"

pkgName :: Package -> Name
pkgName (Pac n _)    = n
pkgName (Brew n _ _) = n
pkgName (Deb n)      = n
