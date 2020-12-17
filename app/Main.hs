module Main where

import Core.Os
import Core.Types
import Core.Format
import Core.Options

import Data.List (isInfixOf)

import Workflow.Git
import Workflow.Input
import Workflow.System
import Workflow.Updates

import System.Directory
import System.FilePath
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if any (isInfixOf "-completion-") args
     then parseOptions >> pure ()
     else bootstrap

bootstrap :: IO ()
bootstrap = do
  -- Make sure git & pip is installed!
  checkDependency "git"

  -- Check for config file
  cfgDir <- getXdgDirectory XdgConfig "dotf"
  exists <- doesFileExist $ cfgDir </> "dotf.yaml"
  pkgSys <- findPackageSystem

  -- Check for package manager extras
  -- Base managers will be indirectly checked by 'findPackageSystem'
  case pkgSys of
    Pacman -> checkDependency "yay"
    _ -> pure ()

  if exists
     then loadDotfConfig >>= run pkgSys
     else mkConfig cfgDir pkgSys >>= run pkgSys

mkConfig :: FilePath -> PkgSystem -> IO Config
mkConfig fp psys = do
  cfg <- inputBootstrap

  let configFile = fp </> "dotf.yaml"
      exampleBuild = fp </> ("example-" ++ installFile psys)

  -- Write initial configs
  createDirectoryIfMissing True fp
  writeFile configFile $ defaultConfig cfg
  writeFile exampleBuild defaultPkgConfig

  return cfg

run :: PkgSystem -> Config -> IO ()
run psys conf = do
  args   <- parseOptions
  plan   <- loadInstallConfig psys conf

  -- App environment
  let env = Env psys conf plan

  -- Check env
  checkEnv env

  -- Workflows
  case args of 
    -- Git functions
    Options d (New opts)   -> gitNewWorkflow env d opts
    Options _ Status       -> gitStatusWorkflow env
    Options _ Diff         -> gitDiffWorkflow env
    Options d (Merge s)    -> gitMergeWorkflow env d s
    Options d (Wip s)      -> gitWipWorkflow env d s
    Options d (Add opts)   -> gitAddWorkflow env d opts
    Options d (Checkout s) -> gitCheckoutWorflow env d s
    Options d (Commit m)   -> gitCommitWorkflow env d m
    Options d (Squash n)   -> gitSquashWorkflow env d n
    Options d Push         -> gitPushWorkflow env d
    Options _ Pull         -> gitPullWorkflow env 
    Options d (Raw s)      -> gitRawWorkflow env d s

    -- Package functions
    Options d Update               -> updateSystem env d
    Options _ (List ListBundles)   -> systemShowBundlesWorkflow env
    Options _ (List ListBranches)  -> gitShowBranchWorkflow env
    Options _ (List ListPkgs)      -> systemShowPackagesWorkflow env
    Options _ (List (ListFiles f)) -> gitShowFilesWorkflow env f
    Options _ (List ListCommitLog) -> gitShowCommitLogWorkflow env

    -- Debug fallthrough
    xs -> print "Not Implemented!" >> print xs
