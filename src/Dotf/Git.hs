module Dotf.Git where

import System.Process
import System.Directory
import System.FilePath

import Data.List (nub, isPrefixOf, intersect, (\\))

gitCall :: [String] -> IO String
gitCall args = readProcess "git" args ""

gitRun :: [String] -> IO ()
gitRun args = callProcess "git" args

dotfCall :: [String] -> IO String
dotfCall args = do
  hd <- getHomeDirectory
  let a1 = "--git-dir=" ++ (hd </> ".dotf")
  let a2 = "--work-tree=" ++ hd
  readCreateProcess ((proc "git" ([a1, a2] ++ args)) { cwd = Just hd }) ""

dotfRun :: [String] -> IO ()
dotfRun args = dotfCall args >> pure ()

dotfStatus :: IO ()
dotfStatus = dotfRun ["status"]

dotfLs :: IO [FilePath]
dotfLs = do
  h <- getHomeDirectory
  l <- lines <$> dotfCall ["ls-files"]
  return $ map (\x -> h ++ "/" ++ x) l

dotfLsDir :: IO [FilePath]
dotfLsDir = do
  h <- getHomeDirectory
  a <- dotfLs
  return $ (nub $ map takeDirectory a) \\ [h]

dotfLsTracked :: Maybe FilePath -> IO [FilePath]
dotfLsTracked Nothing  = dotfLs
dotfLsTracked (Just f) = filter prefix <$> dotfLs
  where prefix x = f `isPrefixOf` x

dotfLsUntracked :: Maybe FilePath -> IO [FilePath]
dotfLsUntracked (Just d) = do
  tracked <- dotfLsTracked $ Just d
  all     <- lsAbsolute d
  return $ all \\ tracked
dotfLsUntracked Nothing  = do
  tracked <- dotfLs
  let dirs = nub $ map takeDirectory tracked
  all     <- mapM lsAbsolute dirs
  return $ (nub $ concat all) \\ tracked

filterByTracked :: [FilePath] -> IO ([FilePath], [FilePath])
filterByTracked files = do
  tracked <- dotfLsTracked Nothing
  return (files \\ tracked, files `intersect` tracked)

newBareRepo :: IO ()
newBareRepo = do
  h <- getHomeDirectory
  _ <- gitRun ["init", "--bare", h </> ".dotf"]
  dotfRun ["config", "--local", "status.showUntrackedFiles", "no"]

attachRepo :: String -> IO ()
attachRepo url = do
  h <- getHomeDirectory
  _ <- gitRun ["clone", "--bare", url, h </> ".dotf"]
  _ <- dotfRun ["checkout"]
  dotfRun ["config", "--local", "status.showUntrackedFiles", "no"]

-----------
-- Utils --
-----------

lsAbsolute :: FilePath -> IO [FilePath]
lsAbsolute p = map f <$> listDirectory p
  where f filename = p </> filename
