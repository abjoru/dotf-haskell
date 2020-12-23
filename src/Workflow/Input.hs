{-# LANGUAGE QuasiQuotes #-}
module Workflow.Input where

import Core.Types
import qualified Core.Term as Term

import Data.String.Interpolate (i, __i)

import System.Directory
import System.FilePath ((</>))
import System.Console.Pretty

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
  h  <- getHomeDirectory
  d  <- getXdgDirectory XdgConfig "dotf"
  ic <- supportsPretty

  let dataPath = h </> ".local" </> "share"
      barePath = h </> ".dotf"

  Term.info [__i|Looks like this is the first time running DotF.
                 You will need a configuration file so that DotF knows where to find stuff.
                 Please answer the following questions truthfully..|]

  ah <- dBool (qHeadless ic) False
  ag <- dPath dataPath $ qGitPath ic dataPath
  ar <- dPath barePath $ qBarePath ic barePath

  return $ Config ah ag h d ar Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    where qHeadless True = "\n1. Is this a headless system? (y/" ++ style Bold "N" ++ ")"
          qHeadless _    = "\n1. Is this a headless system? (y/N)"

          qGitPath True p = "\n2. Where do you want to store Git cloned packages? (" ++ style Bold p ++ ")"
          qGitPath _ p    = [i|\n2. Where do you want to store Git cloned packages? (#{p})|]

          qBarePath True p = "\n3. Where do you want your Git bare repo config to reside? (" ++ style Bold p ++ ")"
          qBarePath _ p    = [i|\n3. Where do you want your Git bare repo config to reside? (#{p})|]
