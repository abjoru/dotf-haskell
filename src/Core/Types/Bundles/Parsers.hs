{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Bundles.Parsers (
  parseBundleConfig
) where

import           Core.Os
import           Core.Types.Bundles.Types

import           Data.Yaml

import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import qualified Data.Vector              as V

-- Internal config item format
data PkgDesc = PkgDesc
  { pkgName   :: Name
  , pkgIsAur  :: Bool
  , pkgIsHead :: Bool
  , pkgIsCask :: Bool
  } deriving Show

parseBundleConfig :: PkgSystem -> CheckMaybePath -> Value -> Parser BundleConfig
parseBundleConfig sys cmp (Object o) = case (name, desc) of
  (Just a, Just b) -> BundleConfig <$> parseJSON a <*> parseJSON b <*> bundles
  _                -> fail "Unable to parse BundleConfig: "
  where name = HM.lookup "name" o
        desc = HM.lookup "description" o
        bundles = parseBundles sys cmp o

parseBundles :: PkgSystem -> CheckMaybePath -> HM.HashMap T.Text Value -> Parser [Bundle]
parseBundles sys cmp m = mapM (parseBundle sys cmp) items
  where items = filter bundlesOnly $ HM.toList m
        bundlesOnly (k, _) = k /= "name" && k /= "description"

parseBundle :: PkgSystem -> CheckMaybePath -> (T.Text, Value) -> Parser Bundle
parseBundle sys cmp (key, Object o) =
  let name = T.unpack key
      pkgs = parsePackages sys $ HM.lookup "packages" o
      gits = parseGitPackages cmp $ HM.lookup "git" o
      hles = parseJSON $ HM.lookupDefault (Bool False) "headless" o
      scrp = parseJSON $ HM.lookupDefault Null "script" o
      pre  = parseJSON $ HM.lookupDefault Null "pre-install" o
      post = parseJSON $ HM.lookupDefault Null "post-install" o
   in Bundle name <$> hles
                  <*> pkgs
                  <*> gits
                  <*> fmap cmp scrp
                  <*> fmap cmp pre
                  <*> fmap cmp post

parsePackages :: PkgSystem -> Maybe Value -> Parser [Package]
parsePackages sys mv = do
  descs <- parsePkgDescs mv
  return $ map (pkg sys) descs
    where pkg Pacman (PkgDesc n a _ _)   = Pac n a
          pkg Homebrew (PkgDesc n _ h c) = Brew n c h
          pkg Apt (PkgDesc n _ _ _)      = Deb n

parsePkgDescs :: Maybe Value -> Parser [PkgDesc]
parsePkgDescs (Just (Array a)) = mapM parsePkgDesc $ V.toList a
parsePkgDescs _                = return []

parsePkgDesc :: Value -> Parser PkgDesc
parsePkgDesc (String s) = return $ PkgDesc (T.unpack s) False False False
parsePkgDesc (Object o) =
  let name = HM.foldlWithKey' f "<unknown>" o
      aur  = parseJSON $ HM.lookupDefault (Bool False) "aur" o
      cask = parseJSON $ HM.lookupDefault (Bool False) "cask" o
      head = parseJSON $ HM.lookupDefault (Bool False) "head" o
   in PkgDesc name <$> aur <*> head <*> cask
  where f _ k Null = T.unpack k
        f a _ _    = a

parseGitPackages :: CheckMaybePath -> Maybe Value -> Parser [Git]
parseGitPackages cmp (Just (Array a)) = mapM pkg $ V.toList a
  where pkg (Object o) =
          let name   = HM.foldlWithKey' f "<unknown>" o
              url    = parseJSON $ HM.lookupDefault "<missing>" "url" o
              branch = parseJSON $ HM.lookupDefault Null "branch" o
              submod = parseJSON $ HM.lookupDefault (Bool False) "submodules" o
              script = parseJSON $ HM.lookupDefault Null "install" o
              cmd    = parseJSON $ HM.lookupDefault Null "command" o
              tpath  = parseJSON $ HM.lookupDefault Null "target" o
           in Git name <$> url
                       <*> branch
                       <*> submod
                       <*> fmap cmp script
                       <*> cmd
                       <*> fmap cmp tpath

        f _ k Null = T.unpack k
        f a _ _    = a
parseGitPackages _ _ = return []
