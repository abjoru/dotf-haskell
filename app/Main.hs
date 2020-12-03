module Main where

import Dotf.Options
import Dotf.Os
import Dotf.Git
import Dotf.Package
import Dotf.Algebras

main :: IO ()
main = do
  args     <- getOpts
  cfgs     <- loadCfg
  files    <- dotfLs
  contents <- mkBundle files

  case args of Options _ Status -> dotfStatus
               Options d Update -> runUpdate cfgs d contents
               xs                 -> catchAll xs

catchAll :: Options -> IO ()
catchAll o = print "Not Implemented! " >> print o
