module Workflow.Updates (updateSystem) where

import           Core.Options
import qualified Core.Term        as Term
import           Core.Types

import           System.Directory (createDirectoryIfMissing)
import           System.Process

updateSystem :: Env -> DryMode -> IO ()
updateSystem e d = do
  cmds <- mkUpdateCommands e
  _    <- checkDirs e
  mapM_ (run d) $ filter ("" /=) cmds
  where run Dry c    = Term.info c
        run Normal c = system c >> pure ()

checkDirs :: Env -> IO ()
checkDirs (Env _ c _) = do
  createDirectoryIfMissing True $ configGitDirectory c
