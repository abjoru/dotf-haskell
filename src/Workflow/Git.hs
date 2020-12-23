{-# LANGUAGE QuasiQuotes #-}
module Workflow.Git where

import Core.Types
import Core.Format
import Core.Options

import Data.String.Interpolate (i)

import System.Directory
import System.Process (system)

-------------------
-- Git Workflows --
-------------------

gitRawWorkflow :: Env -> DryMode -> String -> IO ()
gitRawWorkflow = gitExec

gitNewWorkflow :: Env -> DryMode -> Maybe String -> IO ()
gitNewWorkflow (Env _ c _) d (Just u) = do
  home <- getHomeDirectory
  exists <- doesPathExist $ configRepoDirectory c
  let c1 = [i|git clone --bare #{u} #{configRepoDirectory c}|]
      c2 = gith (configRepoDirectory c) home "checkout"
      c3 = gith (configRepoDirectory c) home "config --local status.showUntrackedFiles no"
  execute exists [c1, c2, c3]
  where execute True _ = putStrLn [i|Local repo directory exists: #{configRepoDirectory c}|]
        execute False cmds = mapM_ (runCmd d) cmds
gitNewWorkflow (Env _ c _) d _ = do
  home <- getHomeDirectory
  exists <- doesPathExist $ configRepoDirectory c
  let c1 = [i|git init --bare #{configRepoDirectory c}|]
      c2 = gith (configRepoDirectory c) home "config --local status.showUntrackedFiles no"
  execute exists [c1, c2]
  where execute True _ = putStrLn [i|Local repo directory exists: #{configRepoDirectory c}|]
        execute False cmds = mapM_ (runCmd d) cmds

gitStatusWorkflow :: Env -> IO ()
gitStatusWorkflow = gitExecNormal "status -sb"

gitDiffWorkflow :: Env -> IO ()
gitDiffWorkflow = gitExecNormal "diff"

gitMergeWorkflow :: Env -> DryMode -> String -> IO ()
gitMergeWorkflow e d b = gitExec e d [i|merge #{b}|]

gitWipWorkflow :: Env -> DryMode -> String -> IO ()
gitWipWorkflow e d b = gitExec e d [i|checkout -b #{b}|]

gitCheckoutWorflow :: Env -> DryMode -> String -> IO ()
gitCheckoutWorflow e d b = gitExec e d [i|checkout #{b}|]

gitAddWorkflow :: Env -> DryMode -> AddMode -> IO ()
gitAddWorkflow e d AddAll = gitExec e d "add -u"
gitAddWorkflow e d (AddFiles xs) = gitExec e d $ mkString "add " " " "" xs

gitCommitWorkflow :: Env -> DryMode -> String -> IO ()
gitCommitWorkflow e d m = gitExec e d [i|commit -m '#{m}'|]

gitPushWorkflow :: Env -> DryMode -> IO ()
gitPushWorkflow e d = gitExec e d "push"

gitPullWorkflow :: Env -> IO ()
gitPullWorkflow = gitExecNormal "pull"

gitShowFilesWorkflow :: Env -> ListOps -> IO ()
gitShowFilesWorkflow e (ListOps Untracked d) = gitExec e Normal [i|ls-files --others --exclude-standard #{d}|]
gitShowFilesWorkflow e (ListOps Tracked d)   = gitExec e Normal [i|ls-files #{d}|]

gitSquashWorkflow :: Env -> DryMode -> Int -> IO ()
gitSquashWorkflow e d n = gitExec e d [i|rebase -i HEAD~#{n}|]

gitShowCommitLogWorkflow :: Env -> IO ()
gitShowCommitLogWorkflow e = gitExec e Normal "log --oneline"

gitShowBranchWorkflow :: Env -> IO ()
gitShowBranchWorkflow = gitExecNormal "branch -a"

-----------
-- Utils --
-----------

gitExecNormal :: String -> Env -> IO ()
gitExecNormal cmd env = gitExec env Normal cmd

gitExec :: Env -> DryMode -> String -> IO ()
gitExec env dry cmd = gitc env cmd >>= runCmd dry
  where gitc (Env _ c _) cmd = do
          h <- getHomeDirectory
          return $ gith (configRepoDirectory c) h cmd

gith :: FilePath -> FilePath -> String -> String
gith a b c = [i|git --git-dir=#{a} --work-tree=#{b} #{c}|]

runCmd :: DryMode -> String -> IO ()
runCmd Dry cmd = putStrLn cmd
runCmd Normal cmd = system cmd >> pure ()
