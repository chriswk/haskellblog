module Handler.Blog where

import Import

import Data.Time (UTCTime, getCurrentTime)
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Data.Time.Format.Human (humanReadableTime')



entryForm :: Form Article
entryForm = renderDivs $ Article
    <$> areq    textField "Title" Nothing
    <*> lift    (liftIO getCurrentTime)
    <*> areq    nicHtmlField "Content" Nothing

getBlogR :: Handler Html
getBlogR = do
    now <- liftIO $ getCurrentTime
    articles <- runDB $ selectList [] [Desc ArticlePosted]
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "articles")

postBlogR :: Handler Html
postBlogR = do
    ((res, articleWidget),enctype) <- runFormPost entryForm
    case res of
        FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
        _ -> defaultLayout $ do
                setTitle "Please correct the entry form"
                $(widgetFile "articleAddError")
