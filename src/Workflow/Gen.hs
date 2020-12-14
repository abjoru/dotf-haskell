module Workflow.Gen where

import Core.Types
import Core.Format
import Core.Utils

import Data.Yaml

import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(XdgCache))
import System.FilePath ((</>))

import Text.Regex.TDFA

import Network.HostName

genHomepage :: Config -> IO ()
genHomepage (Config _ _ _ _ c (Just h) (Just f) (Just l)) = do
  dest <- getXdgDirectory XdgCache "dotf"
  host <- getHostName
  head <- readFile h
  foot <- readFile f
  cont <- mkLinks host <$> decodeGroups l
  mcss <- maybe (pure Nothing) load c

  -- create dirs & remove old files if neccessary
  createDirectoryIfMissing True dest
  removeFiles [dest </> "index.html", dest </> "homepage.css"]

  writeFile (dest </> "index.html") $ head ++ cont ++ foot
  writeCss (dest </> "homepage.css") mcss
    where load :: FilePath -> IO (Maybe String)
          load f = Just <$> readFile f

          writeCss :: FilePath -> Maybe String -> IO ()
          writeCss p (Just x) = writeFile p x
          writeCss _ _        = pure ()
genHomepage _ = return ()

mkLinks :: HostName -> Either ParseException Groups -> String
mkLinks host (Right (Groups gs)) = foldl (build host) "" gs
  where build :: HostName -> String -> Group -> String
        build h acc (Group n f xs) = 
          if checkHost h f
             then acc ++ pre h ++ buildLinks xs ++ post
             else acc

        checkHost :: HostName -> Maybe String -> Bool
        checkHost h (Just f) = h =~ f :: Bool
        checkHost _ _        = True

        buildLinks :: [Link] -> String
        buildLinks xs = mkString "\n" "\n" "\n" $ map f xs
          where f (Link _ n u) = "<a class='bookmark' href='" ++ u ++ "' target='_blank'>" ++ n ++ "</a>"

        pre n = "\n<div class='bookmark-set'>\n<div class='bookmark-title'>" ++ n ++ "</div>\n<div class='bookmark-inner-container'>"
        post  = "</div>\n</div>\n"
mkLinks _ (Left err) = error $ "Unable to load links: " ++ show err
