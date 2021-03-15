{-# LANGUAGE OverloadedStrings #-}
module Core.Types.Startpage.Parsers where

import           Core.Types.Startpage.Types

import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Data.Yaml

parseGroup :: Value -> Parser Group
parseGroup (Object o) =
  let name   = parseJSON $ HM.lookupDefault "<unknown>" "name" o
      filter = parseJSON $ HM.lookupDefault Null "host-filter" o
      links  = parseLinks $ HM.lookup "links" o
   in Group <$> name <*> filter <*> links
parseGroup _ = fail "Expected object type for Group"

parseLinks :: Maybe Value -> Parser [Link]
parseLinks (Just (Array a)) = mapM link $ V.toList a
  where link (Object o) =
          let id   = HM.foldlWithKey' f "<unknown>" o
              name = parseJSON $ HM.lookupDefault "<unknown>" "name" o
              url  = parseJSON $ HM.lookupDefault "" "url" o
           in Link id <$> name <*> url

        f _ k Null = T.unpack k
        f a _ _    = a
parseLinks _ = pure []
