{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage where

import Control.Exception (catch)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.FileStore
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid, (<>))
import Data.String (IsString)
import Data.Text (Text)
--import Data.UUID.V4
import GHC.Generics(Generic)

(</>) :: (IsString m, Monoid m) => m -> m -> m
x </> y = x <> "/" <> y

data ImageType
  = ItPNG
  | ItJPG
  | ItGIF
    deriving (Eq, Show)

data Project = Project
  { projectSlug  :: Text
  , projectName  :: Text
  , projectDescr :: Text
  , projectImgs  :: [String]
  } deriving (Eq, Show, Generic)

instance FromJSON Project where
instance ToJSON Project where

recover :: forall a. IO (Maybe a) -> IO (Maybe a)
recover action = action `catch` go
  where go :: FileStoreError -> IO (Maybe a)
        go _ = return Nothing

fromValue :: FromJSON b => a -> (b -> a) -> ByteString -> a
fromValue def f v = case decode v of
  Nothing -> def
  Just x  -> f x

portfolioStore :: FileStore
portfolioStore = gitFileStore "portfolio-data"

getProjectNames :: IO [String]
getProjectNames = fmap (fromValue [] id)
                       (retrieve portfolioStore "projects.json" Nothing)

getAllProjects :: IO [Project]
getAllProjects = getProjectNames >>= (fmap catMaybes . mapM getProject)

getProject :: String -> IO (Maybe Project)
getProject name =
  fmap (fromValue Nothing Just)
       (retrieve portfolioStore (name <> ".json") Nothing)

getImage :: String -> IO ByteString
getImage name = retrieve portfolioStore ("images" </> name) Nothing

getType :: String -> Text
getType s
  | ".png" `isSuffixOf` s = "image/png"
  | ".jpg" `isSuffixOf` s = "image/jpeg"
  | ".gif" `isSuffixOf` s = "image/gif"
  | otherwise = error "unrecognized image type requested"

getCSS :: IO ByteString
getCSS = retrieve portfolioStore ("css") Nothing

{-
imageType :: String -> ImageType
imageType "image/gif"  = ItGIF
imageType "image/jpeg" = ItJPG
imageType _            = ItPNG

newImage :: UploadedFile -> IO ()
newImage uf = do
  let typ  = imageType (uf_contentType uf)
  name <- nextRandom
  return ()
-}
