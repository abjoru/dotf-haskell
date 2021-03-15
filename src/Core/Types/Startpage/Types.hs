module Core.Types.Startpage.Types where

data Homepage = Homepage
  { homepageHeader     :: FilePath
  , homepageFooter     :: FilePath
  , homepageLinks      :: FilePath
  , homepageStylesheet :: FilePath
  } deriving Show

newtype Groups = Groups [Group]
  deriving Show

data Group = Group
  { groupName   :: String
  , groupFilter :: Maybe String
  , groupLinks  :: [Link]
  } deriving Show

data Link = Link
  { linkId   :: String
  , linkName :: String
  , linkUrl  :: String
  } deriving Show
