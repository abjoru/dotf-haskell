module Core.Options.Parsers where

import Core.Options.Types

import Options.Applicative

-----------------------------
-- Parsers for CMD options --
-----------------------------

-- Parser for the root `Options` type
parserOptions :: Parser Options
parserOptions = Options <$> parserDryMode <*> parserCommand

-- Parser for the `DryMode` flag
parserDryMode :: Parser DryMode
parserDryMode = flag Normal Dry (  long "dryrun"
                                <> short 'd'
                                <> help "Enable dry run"
                                )

-- Parser for the `GitMode` flag
parserGitMode :: Parser GitMode
parserGitMode = flag Tracked Untracked (  long "untracked"
                                       <> short 'u'
                                       <> help "Show untracked files"
                                       )

-- Parser for the `AddMode` flag
parserAddMode :: Parser AddMode
parserAddMode = parserAddFiles <|> parserAddAll

-- Parser for the `AddFiles` option
parserAddFiles :: Parser AddMode
parserAddFiles = AddFiles <$> some (argument str (metavar "FILES..."))

-- Parser for the `AddAll` option
parserAddAll :: Parser AddMode
parserAddAll = flag' AddAll (long "addall" <> short 'a' <> help "Add all dirty files")

-- Parser for the `Command` sub-command
parserCommand :: Parser Command
parserCommand = hsubparser
  (  command "new" (info parserNew (progDesc "Create (or attach) new git bare repository"))
  <> command "status" (info (pure Status) (progDesc "Show current file tree status"))
  <> command "show" (info parserList (progDesc "Show various information"))
  <> command "update" (info (pure Update) (progDesc "Update managed packages"))
  <> command "diff" (info (pure Diff) (progDesc "Diff changes"))
  <> command "merge" (info parserMerge (progDesc "Merge some branch"))
  <> command "wip" (info parserWip (progDesc "Create a new wip branch"))
  <> command "add" (info parserAdd (progDesc "Add files to index"))
  <> command "co" (info parserCheckout (progDesc "Checkout some branch"))
  <> command "commit" (info parserCommit (progDesc "Commit all changes"))
  <> command "squash" (info parserSquash (progDesc "Squash commits in current branch"))
  <> command "push" (info (pure Push) (progDesc "Push changes upstream"))
  <> command "pull" (info (pure Pull) (progDesc "Pull from upstream"))
  <> command "git" (info parserRaw (progDesc "Run some raw git command"))
  <> command "gen" (info parserGen (progDesc "Generate contents"))
  <> command "compose" (info parseCompose (progDesc "Run docker compose command"))
  )

-- Parser for the `New` command
parserNew :: Parser Command
parserNew = New <$> fmap check parserUpstreamOpt
  where check "" = Nothing
        check xa = Just xa

-- Parser for the 'upstream' option for the `New` command
parserUpstreamOpt :: Parser String
parserUpstreamOpt = strOption (  long "upstream"
                              <> short 'u'
                              <> metavar "GIT_URL"
                              <> value ""
                              <> help "Upstream GIT bare repository URL to attach to"
                              )

-- Parser for the `List` (show) command
parserList :: Parser Command
parserList = List <$> parserListCmd

-- Parser for sub-commands of `List`
parserListCmd :: Parser ListCmds
parserListCmd = hsubparser
  (  command "files" (info parserListFiles (progDesc "Show tracked/untracked files in directory"))
  <> command "bundles" (info (pure ListBundles) (progDesc "Show configured bundles"))
  <> command "branch" (info (pure ListBranches) (progDesc "Show git branches"))
  <> command "pkgs" (info (pure ListPkgs) (progDesc "Show configured packages"))
  <> command "log" (info (pure ListCommitLog) (progDesc "Show short commit log"))
  <> command "docker" (info (pure ListDockerServices) (progDesc "Show managed Docker services"))
  )

parserGen :: Parser Command
parserGen = Generate <$> parserGenCmds

parserGenCmds :: Parser GenCmds 
parserGenCmds = hsubparser
  (  command "homepage" (info (pure GenHomepage) (progDesc "Generate homepage"))
  <> command "compose" (info (pure GenCompose) (progDesc "Generate docker compose / env"))
  <> command "vpn" (info (pure GenPiaVpn) (progDesc "Generate PIA openvpn configs"))
  )

parseCompose :: Parser Command 
parseCompose = Compose <$> hsubparser 
  (  command "up" (info parseComposeUpCmd (progDesc "Run compose up command"))
  <> command "down" (info (pure ComposeDown) (progDesc "Run compose down command"))
  <> command "restart" (info parseComposeRestartCmd (progDesc "Run compose restart command"))
  <> command "pull" (info parseComposePullCmd (progDesc "Run compose pull command"))
  )

parseComposeUpCmd :: Parser ComposeCmds
parseComposeUpCmd = ComposeUp <$> parseComposeServiceArg

parseComposePullCmd :: Parser ComposeCmds
parseComposePullCmd = ComposePull <$> parseComposeServiceArg

parseComposeRestartCmd :: Parser ComposeCmds 
parseComposeRestartCmd = ComposeRestart <$> parseComposeServiceArg

parseComposeServiceArg :: Parser [String]
parseComposeServiceArg = many (argument str (metavar "SERVICE"))

-- Parser for `ListFiles` command
parserListFiles :: Parser ListCmds
parserListFiles = ListFiles <$> parserListOps

-- Parser for `ListOps` options
parserListOps :: Parser ListOps
parserListOps = ListOps <$> parserGitMode
                        <*> argument str (metavar "DIR" <> value ".")

parserMerge :: Parser Command
parserMerge = Merge <$> argument str (metavar "BRANCH")

parserWip :: Parser Command
parserWip = Wip <$> argument str (metavar "BRANCH_NAME")

parserAdd :: Parser Command
parserAdd = Add <$> parserAddMode

parserCheckout :: Parser Command
parserCheckout = Checkout <$> argument str (metavar "BRANCH")

parserCommit :: Parser Command
parserCommit = Commit <$> strOption (  long "message"
                                    <> short 'm'
                                    <> metavar "COMMIT_MSG"
                                    <> help "Message for the commit"
                                    )

parserSquash :: Parser Command
parserSquash = Squash <$> option auto (  long "num-commits"
                                      <> short 'n'
                                      <> metavar "N"
                                      <> help "Squash the last N commits"
                                      )

parserRaw :: Parser Command
parserRaw = Raw <$> argument str (metavar "GIT_OPTS")
