{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Core.Utils where

import Data.String.Interpolate (i)

import System.Process
import System.Exit (ExitCode(..), exitSuccess)
import System.Directory --(getXdgDirectory, XdgDirectory(XdgConfig))

import Control.Monad

-- Runs some script with exit code
runScript :: Maybe FilePath -> IO ExitCode
runScript Nothing  = exitSuccess
runScript (Just s) = system $ "sudo sh " ++ s

-- Runs some script with error output to console
runScript' :: Maybe FilePath -> IO ()
runScript' Nothing  = exitSuccess
runScript' (Just s) = do
  r <- runScript $ Just s
  case r of 
    ExitSuccess -> pure ()
    ExitFailure c -> putStrLn [i|Failed to execute script '#{s}': exit code #{c}|]

-- Runs some command in the given directory
runCmdIn :: FilePath -> String -> IO ()
runCmdIn p c = do 
  r <- system [i|pushd #{p} ; #{c} ; popd|]
  case r of 
    ExitSuccess -> pure ()
    ExitFailure e -> putStrLn [i|Failed to execute cmd: '#{c}': exit code #{e}|]

-- Removes list of files if they exist
removeFiles :: [FilePath] -> IO ()
removeFiles [] = pure ()
removeFiles xs = forM_ xs removeIfExists
  where removeIfExists f = do
          exists <- doesFileExist f
          if exists
             then removeFile f
             else pure ()
