{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Templates where

import           Control.Monad (forM_)
import           Data.Char (toLower)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Lucid
import qualified Storage as S

footer :: Html ()
footer = "copyright 2015 Empress Cortana"

page :: Html () -> Html () -> Html ()
page name contents = html_ $ do
  head_ $ do
    title_ name
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css"]
  body_ $ do
    div_ [id_ "header"] $ h1_ name
    div_ [id_ "main"] $ contents
    div_ [id_ "footer"] $ footer

title :: Text -> Html ()
title t = toHtml ("empress cortana // " <> T.map toLower t)

projectList :: [S.Project] -> Html ()
projectList = page (title "portfolio") . mapM_ shortProj

project :: S.Project -> Html ()
project p = page (title (S.projectName p)) (fullProj p)

loginPage :: Html ()
loginPage = page (title "login") $ do
  form_ [name_ "login", action_ "/login"] $ do
    input_ [type_ "password", name_ "password"]
    input_ [type_ "submit", value_ "login"]

imgUrl :: String -> Text
imgUrl s = pack ("/imgs/" <> s)

projUrl :: Text -> Text
projUrl s = "/project/" <> s

shortProj :: S.Project -> Html ()
shortProj (S.Project { .. }) = div_ [class_ "project"] $ do
  div_ [id_ "name"] $ a_ [href_ (projUrl projectSlug)] $ toHtml projectName
  case projectImgs of
    (img:_) -> div_ [id_ "tile"] $ img_ [src_ (imgUrl img)]
    []      -> return ()

fullProj :: S.Project -> Html ()
fullProj (S.Project { .. }) = div_ [class_ "project"] $ do
  div_ [id_ "descr"] $ toHtml projectDescr
  forM_ projectImgs $ \img ->
    div_ [id_ "tile"] $ img_ [src_ (imgUrl img)]
