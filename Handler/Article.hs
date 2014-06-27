module Handler.Article where

import Import

import Data.Time.Format.Human (humanReadableTime)

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    published <- liftIO $ humanReadableTime $ articlePosted article
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")

