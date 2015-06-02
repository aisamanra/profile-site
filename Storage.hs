{-# LANGUAGE OverloadedStrings #-}

module Storage where

import Data.Aeson
import Data.FileStore
import Data.FileStore.Git

data Project = Project
  { projectSlug  :: Text
  , projectName  :: Text
  , projectDescr :: Text
  , projectImgs  :: [Text]
  } deriving (Eq, Show)

instance Contents (Maybe Project) where
  fromByteString = decode
  toByteString (Just x) = encode x
  toByteString Nothing  = error "should not happen"

portfolioStore :: FileStore
portfolioStore = gitFileStore "portfolio-data"

getProjectByName :: String -> IO (Maybe Project)
getProjectByName name = retrieve (name <> ".json") portfolioStore Nothing

getImage :: String -> IO (Maybe ByteString)
getImage name = retrieve ("images" </> name) portfolioStore Nothing

newImage :: UploadedFile -> ()
newImage = undefined
