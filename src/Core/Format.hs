{-# LANGUAGE QuasiQuotes #-}
module Core.Format where

import Data.List (sort)
import Data.Char (toLower)
import Data.String.Interpolate (i, __i)

import qualified Data.HashMap.Lazy as HML

-- Create a string from an array using the given separator
sep :: String -> [String] -> String
sep _ [] = ""
sep _ [x] = x
sep s (x:xs) = x ++ s ++ sep s xs

-- Create a string from an array using the given separator
-- and pre and post string segments
mkString :: String -> String -> String -> [String] -> String
mkString _ _ _ [] = ""
mkString pre _ post [x] = pre ++ x ++ post
mkString pre s post (x:xs) = pre ++ x ++ s ++ sep s xs ++ post

mkConfigStr :: HML.HashMap String String -> String
mkConfigStr ms = sep "\n" $ sort $ map f $ HML.toList ms
  where f :: (String, String) -> String
        f (k, v) = [i|#{k}=#{v}|]
