module Handler.Archives where

import Import

import Data.Time (getCurrentTime)
import Data.Time.Format.Human (humanReadableTime')

getArchivesR :: Handler Html
getArchivesR = do
    now <- liftIO $ getCurrentTime
    articles <- runDB $ selectList [] [Desc ArticlePosted]

    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "archives")
