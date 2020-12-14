{-# LANGUAGE QuasiQuotes #-}
module Workflow.Git where

import Core.Types
import Core.Format
import Core.Options

import Data.String.Interpolate (i)

import System.Exit
import System.Directory
import System.Process (system)

gith :: FilePath -> FilePath -> String -> String
gith a b c = [i|git --git-dir=#{a} --work-tree=#{b} #{c}|]

gitRawWorkflow :: Env -> DryMode -> String -> IO ()
gitRawWorkflow (Env _ c _) d s = getHomeDirectory >>= exec >> pure ()
  where exec p = system $ gith (repoDirectory c) p s

gitNewWorkflow :: Env -> DryMode -> Maybe String -> IO ()
gitNewWorkflow (Env _ c _) d (Just u) = do
  home <- getHomeDirectory
  exists <- doesPathExist $ repoDirectory c
  let c1 = [i|git clone --bare #{u} #{repoDirectory c}|]
      c2 = gith (repoDirectory c) home "checkout"
      c3 = gith (repoDirectory c) home "config --local status.showUntrackedFiles no"
  execute exists [c1, c2, c3]
    where execute True _ = putStrLn [i|Local repo directory exists: #{repoDirectory c}|]
          execute False cmds = mapM_ (runCmd d) cmds
gitNewWorkflow (Env _ c _) d _ = putStrLn "NOT IMPLEMENTED!"

gitStatusWorkflow :: Env -> IO ()
gitStatusWorkflow (Env _ c _) = getHomeDirectory >>= exec >> pure ()
  where exec :: FilePath -> IO ExitCode
        exec p = system $ gith (repoDirectory c) p "status -sb"

gitDiffWorkflow :: Env -> IO ()
gitDiffWorkflow (Env _ c _) = getHomeDirectory >>= exec >> pure ()
  where exec :: FilePath -> IO ExitCode
        exec p = system $ gith (repoDirectory c) p "diff"

gitMergeWorkflow :: Env -> DryMode -> String -> IO ()
gitMergeWorkflow (Env _ c _) d b = getHomeDirectory >>= exec
  where exec :: FilePath -> IO ()
        exec p = runCmd d $ gith (repoDirectory c) p [i|merge #{b}|]

gitWipWorkflow :: Env -> DryMode -> String -> IO ()
gitWipWorkflow (Env _ c _) d b = getHomeDirectory >>= exec
  where exec :: FilePath -> IO ()
        exec p = runCmd d $ gith (repoDirectory c) p [i|checkout -b #{b}|]

gitCheckoutWorflow :: Env -> DryMode -> String -> IO ()
gitCheckoutWorflow (Env _ c _) d b = getHomeDirectory >>= exec
  where exec :: FilePath -> IO ()
        exec p = runCmd d $ gith (repoDirectory c) p [i|checkout #{b}|]

gitAddWorkflow :: Env -> DryMode -> AddMode -> IO ()
gitAddWorkflow e d AddAll = gitc e "add -u" >>= system >> pure ()
gitAddWorkflow (Env _ c _) d (AddFiles xs) = getHomeDirectory >>= exec
  where exec p = runCmd d $ gith (repoDirectory c) p $ mkString "add " " " "" xs

gitCommitWorkflow :: Env -> DryMode -> String -> IO ()
gitCommitWorkflow (Env _ c _) d m = getHomeDirectory >>= exec
  where exec :: FilePath -> IO ()
        exec p = runCmd d $ gith (repoDirectory c) p ("commit -m '" ++ m ++ "'")

gitPushWorkflow :: Env -> DryMode -> IO ()
gitPushWorkflow (Env _ c _) d = getHomeDirectory >>= exec
  where exec :: FilePath -> IO ()
        exec p = runCmd d $ gith (repoDirectory c) p "push"

gitPullWorkflow :: Env -> IO ()
gitPullWorkflow e = gitc e "pull" >>= system >> pure ()

gitShowFilesWorkflow :: Env -> ListOps -> IO ()
gitShowFilesWorkflow e (ListOps Untracked dir) = do
  cmd <- gitc e $ "ls-files --others --exclude-standard " ++ dir 
  res <- system cmd
  return ()
gitShowFilesWorkflow e (ListOps Tracked dir) = do
  cmd <- gitc e $ "ls-files " ++ dir
  res <- system cmd
  return ()

gitSquashWorkflow :: Env -> DryMode -> Int -> IO ()
gitSquashWorkflow (Env _ c _) d n = getHomeDirectory >>= exec
  where exec p = runCmd d $ gith (repoDirectory c) p ("rebase -i HEAD~" ++ show n)

gitShowCommitLogWorkflow :: Env -> IO ()
gitShowCommitLogWorkflow e = gitc e "log --oneline" >>= system >> pure ()

gitShowBranchWorkflow :: Env -> IO ()
gitShowBranchWorkflow e = gitc e "branch -a" >>= system >> pure ()

gitc :: Env -> String -> IO String
gitc (Env _ c _) cmd = do
  h <- getHomeDirectory
  return $ gith (repoDirectory c) h cmd

runCmd :: DryMode -> String -> IO ()
runCmd Dry cmd = putStrLn cmd
runCmd Normal cmd = system cmd >> pure ()
