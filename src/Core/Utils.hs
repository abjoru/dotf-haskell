{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Core.Utils where

import Data.Aeson (Value)
import qualified Data.Aeson.Extra.Merge as M
import Data.String.Interpolate (i)
import Data.HashMap.Strict as HM

import System.Process
import System.Exit (ExitCode(..), exitSuccess)
import System.Directory 
import System.FilePath ((</>))

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
  r <- system $ mkCmdIn p c
  case r of 
    ExitSuccess -> pure ()
    ExitFailure e -> putStrLn [i|Failed to execute cmd: '#{c}': exit code #{e}|]

-- Make bash command to be executed in some directory
mkCmdIn :: FilePath -> String -> String
mkCmdIn p c = [i|bash -c "pushd #{p} ; #{c} ; popd" > /dev/null 2>&1|]

-- Removes list of files if they exist
removeFiles :: [FilePath] -> IO ()
removeFiles [] = pure ()
removeFiles xs = forM_ xs removeIfExists
  where removeIfExists f = do
          exists <- doesFileExist f
          if exists
             then removeFile f
             else pure ()

type CheckPath = FilePath -> FilePath
type CheckMaybePath = Maybe FilePath -> Maybe FilePath

-- HomeFolder -> DotF folder -> Input Folder -> Result
checkPath :: FilePath -> FilePath -> FilePath -> FilePath
checkPath _ _ f@('/':_) = f
checkPath h _ ('~':rx)  = h ++ rx
checkPath _ d other     = d </> other

-- HomeFolder -> DotF folder -> Maybe Input Folder -> Maybe Result
checkMaybePath :: FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath
checkMaybePath h d (Just p) = Just $ checkPath h d p
checkMaybePath _ _ _        = Nothing

-- Aeson Value merge (left-bias)
merge :: Value -> Value -> Value
merge = M.merge f
  where f r (M.ObjectF a) (M.ObjectF b) = M.ObjectF $ HM.unionWith r a b
        f _ (M.ArrayF a)  (M.ArrayF b)  = M.ArrayF $ a <> b
        f _ a _                         = a
