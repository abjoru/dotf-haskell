{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Startpage.Instances where

import Core.Types.Startpage.Types
import Core.Types.Startpage.Parsers

import Data.Yaml

import qualified Data.Vector as V

instance FromJSON Homepage where
  parseJSON (Object o) = Homepage
    <$> o .: "header"
    <*> o .: "footer"
    <*> o .: "links"
    <*> o .: "stylesheet"
  parseJSON _ = fail "Expected object type for Homepage"

instance FromJSON Groups where
  parseJSON (Array a) = Groups <$> mapM parseGroup (V.toList a)
  parseJSON _         = fail "Expected array type for Groups"
