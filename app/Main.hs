module Main where

import           Core.Format
import           Core.Options
import           Core.Os
import           Core.Templates
import           Core.Types

import qualified Core.Term          as Term

import           Data.List          (isInfixOf)
import           Data.Maybe         (catMaybes)

import           Workflow.Compose
import           Workflow.Gen
import           Workflow.Git
import           Workflow.Input
import           Workflow.Openvpn
import           Workflow.System
import           Workflow.Updates

import           System.Directory
import           System.Environment
import           System.FilePath

-- TODO should be able to run without pkg install yaml for all
-- cmds except update (and perhaps some other ones)
-- Allows for attaching and switching branches and such without
-- errors

main :: IO ()
main = do
  args <- getArgs
  -- allow completion script command to run without bootstrap
  if any (isInfixOf "-completion-") args
     then parseOptions >> pure ()
     else bootstrap

bootstrap :: IO ()
bootstrap = do
  -- Check for config file
  cfgDir <- getXdgDirectory XdgConfig "dotf"
  exists <- doesFileExist $ cfgDir </> "dotf.yaml"
  pkgSys <- findPackageSystem

  -- Check dependencies
  gotGit <- which' "git"
  gotYay <- case pkgSys of Pacman -> which' "yay"
                           _      -> pure Nothing

  case catMaybes [gotGit, gotYay] of
    [] -> if exists
             then loadDotfConfig >>= run pkgSys
             else mkConfig cfgDir pkgSys >>= run pkgSys
    xs -> Term.err $ mkString "Missing dependencies: " ", " "" xs

mkConfig :: FilePath -> PkgSystem -> IO Config
mkConfig fp psys = do
  cfg <- inputBootstrap

  let configFile = fp </> "dotf.yaml"
      exampleFile = fp </> ("example-" ++ installFilename psys)

  -- Write initial configs
  createDirectoryIfMissing True fp
  writeFile configFile $ defaultConfig cfg
  writeFile exampleFile defaultPkgConfig

  return cfg

run :: PkgSystem -> Config -> IO ()
run psys conf = do
  args <- parseOptions
  plan <- loadBundleConfig psys conf

  -- App environment
  let env = Env psys conf plan

  -- Check env
  --checkEnv env

  -- Workflows
  case args of
    -- Git functions
    Options d (New opts)                    -> gitNewWorkflow env d opts
    Options _ Status                        -> gitStatusWorkflow env
    Options _ Diff                          -> gitDiffWorkflow env
    Options d (Merge s)                     -> gitMergeWorkflow env d s
    Options d (Wip s)                       -> gitWipWorkflow env d s
    Options d (Add opts)                    -> gitAddWorkflow env d opts
    Options d (Checkout s)                  -> gitCheckoutWorflow env d s
    Options d (Commit m)                    -> gitCommitWorkflow env d m
    Options d (Squash n)                    -> gitSquashWorkflow env d n
    Options d Push                          -> gitPushWorkflow env d
    Options _ Pull                          -> gitPullWorkflow env
    Options d (Raw s)                       -> gitRawWorkflow env d s

    -- Package functions
    Options d Update                        -> updateSystem env d
    Options _ (List ListBundles)            -> systemShowBundlesWorkflow env
    Options _ (List ListBranches)           -> gitShowBranchWorkflow env
    Options _ (List ListPkgs)               -> systemShowPackagesWorkflow env
    Options _ (List (ListFiles f))          -> gitShowFilesWorkflow env f
    Options _ (List ListCommitLog)          -> gitShowCommitLogWorkflow env
    Options _ (List ListDockerServices)     -> composeShow

    -- Generate files
    Options _ (Generate GenHomepage)        -> genHomepage conf
    Options _ (Generate GenCompose)         -> genCompose conf
    Options _ (Generate GenPiaVpn)          -> genOpenVPN conf

    -- Compose functions
    Options d (Compose (ComposeUp xs))      -> composeUp d xs
    Options d (Compose ComposeDown)         -> composeDown d
    Options d (Compose (ComposePull xs))    -> composePull d xs
    Options d (Compose (ComposeRestart xs)) -> composeRestart d xs
    Options d (Compose ComposeShow)         -> composeShow

    -- Debug fallthrough
    --xs -> print "Not Implemented!" >> print xs
