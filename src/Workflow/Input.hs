{-# LANGUAGE QuasiQuotes #-}
module Workflow.Input where

import Core.Types

import Data.String.Interpolate (i, __i)

import System.Directory
import System.FilePath ((</>))

dBool :: String -> Bool -> IO Bool
dBool q d = val <$> (putStrLn q >> getChar)
  where val 'y' = True
        val 'Y' = True
        val 'n' = False
        val 'N' = False
        val _ = d

qString :: String -> IO (Maybe String)
qString q = val <$> (putStrLn q >> getLine)
  where val "" = Nothing
        val v  = Just v

qPath :: String -> IO (Maybe FilePath)
qPath q = val <$> (putStrLn q >> getLine)
  where val "" = Nothing
        val fp = Just fp

dPath :: FilePath -> String -> IO FilePath
dPath d q = val <$> (putStrLn q >> getLine)
  where val "" = d
        val fp = fp

inputBootstrap :: IO Config
inputBootstrap = do
  h <- getHomeDirectory
  d <- getXdgDirectory XdgConfig "dotf"

  let dataPath = h </> ".local" </> "share"
      barePath = h </> ".dotf"

  putStrLn [__i|Looks like this is the first time running DotF.
                You will need a configuration file so that DotF knows where to find stuff.
                Please answer the following questions truthfully..|]

  ah <- dBool "\n1. Is this a headless system? (y/N)" False
  ag <- dPath dataPath $ "\n" ++ [i|2. Where do you want to store Git cloned packages? (#{dataPath})|]
  ar <- dPath barePath $ "\n" ++ [i|3. Where do you want your Git bare repo config to reside? (#{barePath})|]

  return $ Config ah ag d ar Nothing Nothing Nothing Nothing
