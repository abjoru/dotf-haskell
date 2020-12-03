module Dotf.Options (
  getOpts, DryRun(..), GitMode(..), Command(..), Options(..),
  TargetBranch, AddOpts(..), CommitOpts(..), ListOps(..)
) where

import Options.Applicative

----------------
-- Data Types --
----------------

data DryRun = Normal | Dry deriving Show

data GitMode = Tracked | Untracked deriving Show

data Command = New                      -- Create new bare repo for dotf
             | Status                   -- Show file status
             | Update                   -- Update all packages
             | Diff                     -- Show diff for current branch
             | Merge TargetBranch       -- Merge some branch
             | Wip TargetBranch         -- Create a new WIP branch
             | Checkout TargetBranch    -- Checkout some branch
             | Add AddOpts              -- Add files to index
             | AddAll                   -- Add all dirty files
             | Commit CommitOpts        -- Commit changes
             | Push                     -- Push changes to upstream
             | Generate                 -- Generate contents
             | List ListOps             -- General list command
             deriving Show

type TargetBranch = String

data AddOpts = AddOpts { path :: String } deriving Show

data CommitOpts = CommitOpts { msg :: String } deriving Show

data ListOps = ListOps 
  { gitMode :: GitMode
  , dir :: String
  } deriving Show

data Options = Options 
  { dry :: DryRun
  , cmd :: Command
  } deriving Show

-------------
-- Parsers --
-------------

getOpts :: IO Options
getOpts = execParser $ info (parseOptions <**> helper)
  (fullDesc <> progDesc "This is a test!")

parseOptions :: Parser Options
parseOptions = Options <$> dryRunParser <*> commandParser

dryRunParser :: Parser DryRun
dryRunParser = flag Normal Dry (  long "dryrun"
                               <> short 'd'
                               <> help "Enable dry mode"
                               )

commandParser :: Parser Command
commandParser = hsubparser
  (  command "new" (info cmdNewParser (progDesc "Create new git bare repository"))
  <> command "status" (info cmdStatusParser (progDesc "Show current file status"))
  <> command "update" (info cmdUpdateParser (progDesc "Update managed packages"))
  <> command "diff" (info cmdDiffParser (progDesc "Show active diff"))
  <> command "merge" (info cmdMergeParser (progDesc "Merge some branch"))
  <> command "wip" (info cmdWipParser (progDesc "Creates a new WIP branch"))
  <> command "co" (info cmdCheckoutParser (progDesc "Checkout some branch"))
  <> command "add" (info cmdAddParser (progDesc "Add files to index"))
  <> command "addall" (info cmdAddAllParser (progDesc "Add all dirty files"))
  <> command "commit" (info cmdCommitParser (progDesc "Commit all changes"))
  <> command "push" (info cmdPushParser (progDesc "Push changes to upstream"))
  <> command "gen" (info cmdGenerateParser (progDesc "Generate contents"))
  <> command "show" (info cmdListParser (progDesc "Show files"))
  )

cmdNewParser :: Parser Command
cmdNewParser = pure New

cmdStatusParser :: Parser Command
cmdStatusParser = pure Status

cmdUpdateParser :: Parser Command
cmdUpdateParser = pure Update

cmdDiffParser :: Parser Command
cmdDiffParser = pure Diff

cmdMergeParser :: Parser Command
cmdMergeParser = Merge <$> targetBranchParser

cmdWipParser :: Parser Command
cmdWipParser = Wip <$> targetBranchParser

cmdCheckoutParser :: Parser Command
cmdCheckoutParser = Checkout <$> targetBranchParser

cmdAddParser :: Parser Command
cmdAddParser = Add <$> addOptsParser

cmdAddAllParser :: Parser Command
cmdAddAllParser = pure AddAll

cmdCommitParser :: Parser Command
cmdCommitParser = Commit <$> commitOptsParser

cmdPushParser :: Parser Command
cmdPushParser = pure Push

cmdGenerateParser :: Parser Command
cmdGenerateParser = pure Generate

cmdListParser :: Parser Command
cmdListParser = List <$> listOpsParser

targetBranchParser :: Parser TargetBranch
targetBranchParser = argument str (metavar "TARGETBRANCH")

addOptsParser :: Parser AddOpts
addOptsParser = AddOpts <$> argument str (metavar "FILE")

listOpsParser :: Parser ListOps
listOpsParser = ListOps
  <$> flag Tracked Untracked (  long "untracked"
                             <> short 'u'
                             <> help "List untracked files in target"
                             )
  <*> argument str (metavar "FILE")

commitOptsParser :: Parser CommitOpts
commitOptsParser = CommitOpts
  <$> strOption (  long "message"
                <> short 'm'
                <> help "Commit message"
                )
