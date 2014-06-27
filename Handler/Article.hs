module Handler.Article where

import Import

import Data.Time.Format.Human (humanReadableTime)

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    published <- liftIO $ do
    	published' <- humanReadableTime $ articlePosted article
    	return published'
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")
