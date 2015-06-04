{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Templates where

import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import           Lucid
import qualified Storage as S

footer :: Html ()
footer = "© Empress Cortana 2015"

page :: Html () -> Html () -> Html ()
page name contents = html_ $ do
  head_ $ do
    title_ name
  body_ $ do
    div_ [id_ "header"] $ h1_ name
    div_ [id_ "main"] $ contents
    div_ [id_ "footer"] $ footer

projectList :: [S.Project] -> Html ()
projectList = page "empress cortana // portfolio" . mapM_ shortProj

imgUrl :: String -> Text
imgUrl s = pack ("imgs/" <> s)

projUrl :: Text -> Text
projUrl s = "project/" <> s

shortProj :: S.Project -> Html ()
shortProj (S.Project { .. }) = div_ [class_ "project"] $ do
  div_ [id_ "name"] $ a_ [href_ (projUrl projectSlug)] $ toHtml projectName
  case projectImgs of
    (img:_) -> div_ [id_ "tile"] $ img_ [src_ (imgUrl img)]
    []      -> return ()
