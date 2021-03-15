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
             | Raw String        -- raw '<git command args>'
             | Generate GenCmds
             | Compose ComposeCmds
             deriving Show

-- List sub-commands (i.e. show options)
data ListCmds = ListFiles ListOps
              | ListBundles
              | ListPkgs
              | ListBranches
              | ListCommitLog
              | ListDockerServices
              deriving Show

data GenCmds = GenHomepage    -- Generate homepage
             | GenCompose     -- Generate docker compose and env files
             | GenPiaVpn      -- Generate PIA openvpn configs
             deriving Show

-- Docker Compose commands
data ComposeCmds = ComposeUp [String]
                 | ComposeDown
                 | ComposeRestart [String]
                 | ComposePull [String]
                 deriving Show

data ListOps = ListOps
  { gitMode :: GitMode
  , dir     :: FilePath
  } deriving Show
