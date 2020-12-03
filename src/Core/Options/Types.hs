module Core.Options.Types where

-- Dry/Normal command mode
data DryMode = Normal | Dry deriving Show

-- Tracked/Untracked file list mode
data GitMode = Tracked | Untracked deriving Show

-- Git add command mode
data AddMode = AddFiles [FilePath] | AddAll deriving Show

-- Root
data Options = Options
  { dry :: DryMode
  , cmd :: Command
  } deriving Show

-- Top-level commands
data Command = New (Maybe String)
             | Status
             | List ListCmds
             | Update
             | Diff
             | Merge String      -- merge <branch>
             | Wip String        -- wip <branch>
             | Add AddMode       -- add -u | add <file>
             | Checkout String   -- co <branch>
             | Commit String     -- Commit -m <string>
             | Squash Int        -- Interactive rebase: HEAD~<int>
             | Push              -- push to upstream
             | Pull              -- pull from upstream
             | Raw String        -- raw '<raw git command args>'
             | Generate
             deriving Show

data ListCmds = ListFiles ListOps
              | ListBundles
              | ListPkgs
              | ListBranches
              | ListCommitLog
              deriving Show

data ListOps = ListOps
  { gitMode :: GitMode
  , dir :: FilePath
  } deriving Show
