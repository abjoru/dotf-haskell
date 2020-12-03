module Core.Options.Parsers where

import Core.Options.Types

import Options.Applicative

parserOptions :: Parser Options
parserOptions = Options <$> parserDryMode <*> parserCommand

parserDryMode :: Parser DryMode
parserDryMode = flag Normal Dry (  long "dryrun"
                                <> short 'd'
                                <> help "Enable dry run"
                                )

parserGitMode :: Parser GitMode
parserGitMode = flag Tracked Untracked (  long "untracked"
                                       <> short 'u'
                                       <> help "Show untracked files"
                                       )

parserAddMode :: Parser AddMode
parserAddMode = parserAddFiles <|> parserAddAll

parserAddFiles :: Parser AddMode
parserAddFiles = AddFiles <$> some (argument str (metavar "FILES..."))

parserAddAll :: Parser AddMode
parserAddAll = flag' AddAll (long "addall" <> short 'a' <> help "Add all dirty files")

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
  <> command "gen" (info (pure Generate) (progDesc "Generate contents"))
  )

parserNew :: Parser Command
parserNew = New <$> fmap check parserUpstreamOpt
  where check "" = Nothing
        check xa = Just xa

parserUpstreamOpt :: Parser String
parserUpstreamOpt = strOption (  long "upstream"
                              <> short 'u'
                              <> metavar "GIT_URL"
                              <> value ""
                              <> help "Upstream GIT bare repository URL to attach to"
                              )

parserList :: Parser Command
parserList = List <$> parserListCmd

parserListCmd :: Parser ListCmds
parserListCmd = hsubparser
  (  command "files" (info parserListFiles (progDesc "Show tracked/untracked files in directory"))
  <> command "bundles" (info (pure ListBundles) (progDesc "Show configured bundles"))
  <> command "branch" (info (pure ListBranches) (progDesc "Show git branches"))
  <> command "pkgs" (info (pure ListPkgs) (progDesc "Show configured packages"))
  <> command "log" (info (pure ListCommitLog) (progDesc "Show short commit log"))
  )

parserListFiles :: Parser ListCmds
parserListFiles = ListFiles <$> parserListOps

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
