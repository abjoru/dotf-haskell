{-# LANGUAGE OverloadedStrings #-}
module Dotf.Utils where

import Dotf.Types
import Dotf.Package

import System.Process
import System.Exit (ExitCode, exitSuccess)

mkString' :: String -> [String] -> String
mkString' _ [] = ""
mkString' _ [x] = x
mkString' s (x:xs) = x ++ s ++ (mkString' s xs)

mkString :: String -> String -> String -> [String] -> String
mkString _ _ _ [] = ""
mkString _ _ _ [x] = x
mkString pre s post (x:xs) = pre ++ x ++ s ++ (mkString' s xs) ++ post

mkPutStrLn :: String -> String -> String -> [String] -> IO ()
mkPutStrLn _ _ _ [] = pure ()
mkPutStrLn pre s post xs = putStrLn $ mkString pre s post xs

mkPutStrLn' :: String -> String -> Maybe String -> IO ()
mkPutStrLn' _ _ Nothing = pure ()
mkPutStrLn' pre post (Just x) = putStrLn $ pre ++ x ++ post

lookupPkgs :: (Package -> Maybe [String]) -> Package -> [String]
lookupPkgs f p = case (f p) of
  Just x  -> x
  Nothing -> []

lookupPkgs' :: (Package -> Maybe [String]) -> [Bundle] -> [String]
lookupPkgs' f [PkgBundle _ _ p] = lookupPkgs f p
lookupPkgs' f ((PkgBundle _ _ p):xs) = (lookupPkgs f p) ++ (lookupPkgs' f xs)
lookupPkgs' _ _ = []

-- Runs some script
runScript :: Maybe FilePath -> IO ExitCode
runScript Nothing  = exitSuccess
runScript (Just s) = system $ "sudo sh " ++ s
