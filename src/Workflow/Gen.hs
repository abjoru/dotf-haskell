{-# LANGUAGE QuasiQuotes #-}
module Workflow.Gen where

import           Core.Format
import           Core.Os
import           Core.Types

import           Data.List.Split         (chunksOf)
import           Data.String.Interpolate (__i, i)
import           Data.Yaml

import           System.Directory        (XdgDirectory (XdgCache),
                                          createDirectoryIfMissing,
                                          getXdgDirectory)
import           System.FilePath         ((</>))

import           Text.Regex.PCRE

import           Network.HostName

genHomepage :: Config -> IO ()
genHomepage c@(Config _ _ h d _ _ (Just hp) _) = do
  dest <- getXdgDirectory XdgCache "dotf"
  host <- getHostName
  head <- readFile $ homepageHeader hp
  foot <- readFile $ homepageFooter hp
  css  <- readFile $ homepageStylesheet hp
  cont <- mkGroups host <$> decodeHomepageGroups (homepageLinks hp)

  -- remove the old stuff
  putStrLn [i|Checking path #{dest}...|]
  createDirectoryIfMissing True dest
  removeFiles [dest </> "homepage.html", dest </> "homepage.css"]

  -- write the new stuff
  putStrLn "Assembling and writing homepage..."
  writeFile (dest </> "homepage.html") $ head ++ cont ++ foot
  writeFile (dest </> "homepage.css") css
  putStrLn [i|Homepage written to #{dest </> "homepage.html"}|]
genHomepage _ = putStrLn "No homepage configuration found!"

-----------
-- Utils --
-----------

mkGroups :: HostName -> Either ParseException Groups -> String
mkGroups host (Right (Groups gs)) = foldl (assem host) "" $ chunksOf 6 gs
  where assem h acc gx = acc ++ [__i|<div class="bookmark-container">
                                  #{foldl (build h) "" gx}
                                </div>|]

        build :: HostName -> String -> Group -> String
        build h acc (Group n f xs) =
          if checkHost h f
             then acc ++ mkLinks n xs
             else acc
mkGroups _ (Left err) = error $ "Unable to load links: " ++ show err

mkLinks :: String -> [Link] -> String
mkLinks n xs = mkString (mkPre n) "\n" mkPost $ map mkLink xs
  where mkPre :: String -> String
        mkPre n = [__i|<div class="bookmark-set">
                        <div class="bookmark-title">#{n}</div>
                        <div class="bookmark-inner-container">
                      |]

        mkPost = "</div>\n</div>\n"

mkLink :: Link -> String
mkLink (Link _ n u) = [i|<a class="bookmark" href="#{u}" target="_blank">#{n}</a>|]

-- Check hostname against optional regex pattern
checkHost :: HostName -> Maybe String -> Bool
checkHost h (Just s) = h =~ s :: Bool
checkHost _ _        = True
