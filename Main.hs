{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Lucid (Html, renderBS)
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

imageR :: Path '[String]
imageR = "imgs" <//> var

editR :: Path '[]
editR = "edit"

editProjectR :: Path '[String]
editProjectR = "edit" <//> "project" <//> var

lucid :: MonadIO m => Html () -> ActionT m a
lucid = lazyBytes . renderBS

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  get root $ do
    projects <- liftIO S.getAllProjects
    lucid (T.projectList projects)
  get projectR $ \ p -> do
    Just project <- liftIO (S.getProject p)
    lucid (T.project project)
  get loginR $ do
    lucid T.loginPage
  get imageR $ \ i -> do
    setHeader "Content-Type" (S.getType i)
    d <- liftIO (S.getImage i)
    lazyBytes d
  get "css" $ do
    setHeader "Content-Type" "text/css"
    liftIO S.getCSS >>= lazyBytes
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
