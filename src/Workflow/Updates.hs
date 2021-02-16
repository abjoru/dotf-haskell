module Workflow.Updates (updateSystem) where

import Core.Types
import Core.Options
import qualified Core.Term as Term

import System.Process

updateSystem :: Env -> DryMode -> IO ()
updateSystem e d = do
  cmds <- mkUpdateCommands e
  mapM_ (run d) $ filter ("" /=) cmds
  where run Dry c = Term.info c
        run Normal c = system c >> pure ()
