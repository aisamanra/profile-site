{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad (forM_)
import           Data.HashMap (toList)
-- import qualified Storage   as S
-- import qualified Templates as T
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

projectR :: Path '[Text]
projectR = "project" <//> var

loginR :: Path '[]
loginR = "login"

editR :: Path '[]
editR = "edit"

editProjectR :: Path '[Text]
editProjectR = "edit" <//> "project" <//> var

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  get Root $ do
    projs <- S.getProjects
    text "foo"
  get projectR $ \ p -> do
    text "whoo"
  post Root $ do
    fs <- files
    forM_ (toList fs) $ \ (k, v) -> do
      liftIO $ putStrLn $ "name" ++ show k
      liftIO $ putStrLn $ "uf_name" ++ show (uf_name v)
      liftIO $ putStrLn $ "uf_tempLocation" ++ show (uf_tempLocation v)
    text "blah"
