{-# LANGUAGE QuasiQuotes #-}
module Workflow.System where

import           Core.Options
import           Core.Types

import           Data.Foldable
import           Data.String.Interpolate (i)

systemShowBundlesWorkflow :: Env -> IO ()
systemShowBundlesWorkflow (Env _ _ (BundleConfig _ _ xs)) = forM_ xs out
  where out (Bundle n h _ _ _ _ _) = putStrLn [i|#{n} (Headless:#{h})|]

systemShowPackagesWorkflow :: Env -> IO ()
systemShowPackagesWorkflow (Env Pacman _ c)   = forM_ (pkgName <$> allPkgs c) putStrLn
systemShowPackagesWorkflow (Env Homebrew _ c) = forM_ (pkgName <$> allPkgs c) putStrLn
systemShowPackagesWorkflow (Env Apt _ c)      = forM_ (pkgName <$> allPkgs c) putStrLn

-----------
-- Utils --
-----------

allPkgs :: BundleConfig -> [Package]
allPkgs ic = foldl f [] $ bundles ic
  where f acc b = acc ++ bundlePackages b
