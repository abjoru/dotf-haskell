{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Parsers where

import Core.Types.Types

import Data.Yaml

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- Parse an array of `Bundle`s
parseBundles :: HM.HashMap T.Text Value -> Parser [Bundle]
parseBundles m = mapM parseBundle items
  where items = filter bundlesOnly $ HM.toList m
        bundlesOnly (k, _) = k /= "name" && k /= "description"

-- Parse a single `Bundle` instance
parseBundle :: (T.Text, Value) -> Parser Bundle
parseBundle (key, Object o) =
  let name = T.unpack key
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

-- Parse an array of `Pkg`s
parsePkgs :: Maybe Value -> Parser [Pkg]
parsePkgs (Just (Array a)) = mapM parsePkg $ V.toList a
parsePkgs _ = return []

-- Parse a single `Pkg` instance
parsePkg :: Value -> Parser Pkg
parsePkg (String s) = return $ Pkg (T.unpack s) False False 
parsePkg (Object o) =
  let name = HM.foldlWithKey' f "<unknown>" o
      aur  = parseJSON $ HM.lookupDefault (Bool False) "aur" o
      cask = parseJSON $ HM.lookupDefault (Bool False) "cask" o
   in Pkg name <$> aur <*> cask
  where f _ k Null = T.unpack k
        f a _ _    = a

-- Parse an array of `GitPkg`s
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

        f _ k Null = T.unpack k
        f a _ _    = a
parseGitPkgs _ = return []

-- Parse an array of PIP package names
parsePipPkgs :: Maybe Value -> Parser [String]
parsePipPkgs (Just a@(Array _)) = parseJSON a
parsePipPkgs _                  = return []

-- Parse a homepage `Group` of links
parseGroup :: Value -> Parser Group 
parseGroup (Object o) =
  let name   = parseJSON $ HM.lookupDefault "<unknown>" "name" o
      filter = parseJSON $ HM.lookupDefault Null "host-filter" o
      links  = parseLinks $ HM.lookup "links" o
   in Group <$> name <*> filter <*> links
parseGroup _ = fail "Expected Object for Group"

-- Parse an array of `Link`s
parseLinks :: Maybe Value -> Parser [Link]
parseLinks (Just (Array a)) = mapM link $ V.toList a
  where link (Object o) =
          let id   = HM.foldlWithKey' f "<unknown>" o
              name = parseJSON $ HM.lookupDefault "<unknown>" "name" o
              url  = parseJSON $ HM.lookupDefault "" "url" o
           in Link id <$> name <*> url

        f _ k Null = T.unpack k
        f a _ _    = a
parseLinks _ = return []
