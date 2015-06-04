{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.HashMap.Strict (toList)
import           Lucid (renderBS)
import qualified Storage   as S
import qualified Templates as T
import           Web.Spock.Safe

{- /
    [ image ] [ project ]
    [ image ] [ project ]
 - /project/:blah
    [ image ] [ text ]
    [ image ]
    prev next main
     [ archives ]
 - /login
     [ password ]
 - /edit

-}

projectR :: Path '[String]
projectR = "project" <//> var

loginR :: Path '[]
loginR = "login"

editR :: Path '[]
editR = "edit"

editProjectR :: Path '[String]
editProjectR = "edit" <//> "project" <//> var

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  get root $ do
    projects <- liftIO S.getAllProjects
    lazyBytes (renderBS $ T.projectList projects)
  get projectR $ \ p -> do
    project <- liftIO (S.getProject p)
    text (T.pack $ show project)
{-
  post root $ do
    fs <- files
    forM_ (toList fs) $ \ (k, v) -> do
      liftIO $ do
        putStrLn $ "name" ++ show k
        putStrLn $ "uf_name" ++ show (uf_name v)
        putStrLn $ "uf_contentType" ++ show (uf_contentType v)
        putStrLn $ "uf_tempLocation" ++ show (uf_tempLocation v)
    text "blah"
-}
