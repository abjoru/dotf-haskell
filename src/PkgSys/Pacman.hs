module PkgSys.Pacman where

import Dotf.Types
import Dotf.Utils
import Dotf.Options (DryRun(..))
import Dotf.Package

import Data.List ((\\))

import System.Process
import System.Exit (ExitCode, exitSuccess)

listNames :: IO [String]
listNames = do
  xs <- readProcess "pacman" ["-Q"] ""
  return $ map f $ lines xs
    where f line = head $ words line

updateSystem :: IO ExitCode
updateSystem = system "sudo pacman -Syyu"

installPkgs :: [String] -> IO ExitCode
installPkgs []   = exitSuccess
installPkgs pkgs = system $ "sudo pacman -S " ++ (mkString' " " pkgs)

installAur :: [String] -> IO ExitCode
installAur []   = exitSuccess
installAur pkgs = system $ "yay -S " ++ (mkString' " " pkgs)

updateContent :: DryRun -> Stats -> Bundle -> IO ()
updateContent Dry _ (ScriptBundle f) = putStrLn $ "sudo sh " ++ f
updateContent Normal _ (ScriptBundle f) = system ("sudo sh " ++ f) >> pure ()
updateContent Dry st (PkgBundle p1 p2 p) = do 
  _ <- mkPutStrLn' "sudo sh " "" p1
  _ <- mkPutStrLn "sudo pacman -S " " " "" $ newPacman (pacman p)
  _ <- mkPutStrLn "yay -S " " " "" $ newAur (aur p)
  _ <- mkPutStrLn' "sudo sh " "" p2
  return ()
    where newPacman (Just x) = x \\ (systemPkgs st)
          newPacman Nothing  = []
          newAur (Just x)    = x \\ (systemPkgs st)
          newAur Nothing     = []
updateContent Normal st (PkgBundle p1 p2 p) = do
  _ <- runScript p1
  _ <- installPkgs $ newPacman (pacman p)
  _ <- installAur $ newAur (aur p)
  _ <- runScript p2
  return ()
    where newPacman (Just x) = x \\ (systemPkgs st)
          newPacman Nothing  = []
          newAur (Just x)    = x \\ (systemPkgs st)
          newAur Nothing     = []
