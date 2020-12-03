{-# LANGUAGE OverloadedStrings #-}
module Dotf.Package where

import Dotf.Types

import qualified Data.ByteString as BS
import Data.Yaml
import Data.Maybe (isJust)
import Data.List (isSuffixOf)

import System.Directory
import System.FilePath

mkBundle :: [FilePath] -> IO [Bundle]
mkBundle fps = do
  let pkgs = filter byPkgs fps
  let inst = filter byInst fps
  mapM mkBundle' (pkgs ++ inst)
    where byPkgs v = ".pkg" `isSuffixOf` v
          byInst v = "/install.sh" `isSuffixOf` v

mkBundle' :: FilePath -> IO Bundle
mkBundle' fp
  | takeExtension fp == ".pkg" = fromPkg fp
  | takeFileName fp == "install.sh" = fromInstallScript fp
  | otherwise = error $ "Cannot create contents from " ++ fp

fromPkg :: FilePath -> IO Bundle
fromPkg fp = do
  let parent = takeDirectory fp
  bytes     <- BS.readFile fp
  maybePre  <- findFile [parent] "preinstall.sh"
  maybePost <- findFile [parent] "postinstall.sh"
  let pkg = decodeEither' bytes
  case pkg of
    Right c -> return $ PkgBundle maybePre maybePost c
    Left er -> error $ "Cannot create pkg bundle from " ++ fp ++ ": " ++ (show er)

fromInstallScript :: FilePath -> IO Bundle
fromInstallScript fp = return $ ScriptBundle fp 

filterByOs :: String -> [Bundle] -> [Bundle]
filterByOs os xs = filter (f os) xs
  where f "pacman" (PkgBundle _ _ p) = isJust (pacman p) || isJust (aur p)
        f "brew" (PkgBundle _ _ p)   = isJust (brew p) || isJust (brewCask p)
        f "apt" (PkgBundle _ _ p)    = isJust (apt p)
        f _ _                        = False

-- getAllPackages :: String -> [Contents] -> [String]
-- getAllPackages os xs = foldl (collectPkgs os) [] (filterByOs os xs)

-- getAllPreInstallScripts :: String -> [Contents] -> [FilePath]
-- getAllPreInstallScripts os xs = foldl collectPreScripts [] $ filterByOs os xs

-- getAllPostInstallScripts :: String -> [Contents] -> [FilePath]
-- getAllPostInstallScripts os xs = foldl collectPostScripts [] $ filterByOs os xs

-- getAllInstallScripts :: [Contents] -> [FilePath]
-- getAllInstallScripts xs = map (\x -> file x) $ filter (\x -> takeExtension (file x) == ".sh") xs

-----------
-- Utils --
-----------

-- collectPkgs :: String -> [String] -> Contents -> [String]
-- collectPkgs os acc (Contents _ _ _ (Just p)) = acc ++ (getAll os p)

-- collectPreScripts :: [FilePath] -> Contents -> [FilePath]
-- collectPreScripts acc (Contents _ (Just f) _ _) = acc ++ [f]

-- collectPostScripts :: [FilePath] -> Contents -> [FilePath]
-- collectPostScripts acc (Contents _ _ (Just f) _) = acc ++ [f]

-- getAll :: String -> Package -> [String]
-- getAll "pacman" (Package _ _ Nothing (Just b) _ _ _) = unpackAll b
-- getAll "pacman" (Package _ _ (Just a) Nothing _ _ _) = unpackAll a
-- getAll "pacman" (Package _ _ (Just a) (Just b) _ _ _) = unpackAll (a ++ b)
-- getAll "brew" (Package _ _ _ _ _ Nothing (Just b)) = unpackAll b
-- getAll "brew" (Package _ _ _ _ _ (Just a) Nothing) = unpackAll a
-- getAll "brew" (Package _ _ _ _ _ (Just a) (Just b)) = unpackAll (a ++ b)
-- getAll "apt" (Package _ _ _ _ (Just a) _ _) = unpackAll a
-- getAll _ _ = []
