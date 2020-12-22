{-# LANGUAGE QuasiQuotes #-}
module Workflow.Gen where

import Core.Types
import Core.Format
import Core.Utils

import Data.Yaml
import Data.String.Interpolate (i, __i)

import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(XdgCache))
import System.FilePath ((</>))

import Text.Regex.PCRE

import Network.HostName

genHomepage :: Config -> IO ()
genHomepage c@(Config _ _ _ d _ (Just hp)) = do
  dest <- getXdgDirectory XdgCache "dotf"
  host <- getHostName
  head <- readFile $ toAbsolute c (header hp)
  foot <- readFile $ toAbsolute c (footer hp)
  css  <- readFile $ toAbsolute c (css hp)
  cont <- mkGroups host <$> decodeGroups (toAbsolute c (links hp))

  -- remove the old stuff
  putStrLn [i|Checking path #{dest}...|]
  createDirectoryIfMissing True dest
  removeFiles [dest </> "homepage.html", dest </> "homepage.css"]

  -- write the new stuff
  putStrLn "Assembling and writing homepage..."
  writeFile (dest </> "homepage.html") $ head ++ cont ++ foot
  writeFile (dest </> "homepage.css") css
  putStrLn [i|Homepage written to #{dest </> "homepage.html"}|]
genHomepage _ = pure ()

-----------
-- Utils --
-----------

mkGroups :: HostName -> Either ParseException Groups -> String
mkGroups host (Right (Groups gs)) = foldl (build host) "" gs
  where build :: HostName -> String -> Group -> String
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
