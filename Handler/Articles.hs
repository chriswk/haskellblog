module Handler.Articles
    ( getArticleR
    , postArticleR 
    , getManageArticlesR
    , postManageArticlesR
    , getNewArticleR
    , postNewArticleR
    , getEditArticleR
    , postEditArticleR
    , getDelPostR
    ) where

import Import

import Prelude (init, last)
import Helpers.Article
import qualified Data.Text as T

getArticleR :: Text -> Handler Html
getArticleR slug = do
    (article,tags,mprev,mnext) <- runDB $ do
        (article',tags') <- getArticle404 slug
        mprev' <- getPreviousArticle article'
        mnext' <- getNextArticle article'

        return $ (article',tags',mprev',mnext')
        
    let published = liftIO $ humanReadableTime $ articlePosted article

    defaultLayout $ do
        setTitle $ toHtml slug
        addKeywords $ map tagName tags
        $(widgetFile "article/show")

postArticleR :: Text -> Handler Html
postArticleR = getArticleR

getManageArticlesR :: Handler Html
getManageArticlesR = do
    articles <- runDB $ selectList [] [Desc ArticlePosted]
    tags <- fmap (map entityVal) $ runDB $ selectList [] [Asc TagName]

    now <- liftIO getCurrentTime

    defaultLayout $ do
        setTitle "Manage articles"
        $(widgetFile "article/index")

postManageArticlesR :: Handler Html
postManageArticlesR = getManageArticlesR

getNewArticleR :: Handler Html
getNewArticleR = do
    now <- liftIO getCurrentTime

    mslug <- runInputGet $ iopt textField "slug"

    ((res,form),enctype) <- runFormPost $ articleForm (fmap (mkStub now) mslug)

    case res of
        FormSuccess af -> upsertArticle af
        _              -> return ()

    where
        mkStub :: UTCTime -> Text -> (Article,[Tag])
        mkStub now slug = (Article
            { articleSlug = slug
            , articlePosted = now
            , articleTitle = titleize slug
            , articleContent = ""
            , articleDraft = True
            }, [])
        titleize :: Text -> Text
        titleize = T.unwords . map capitalize . T.words . T.map score

        capitalize :: Text -> Text
        capitalize = (\(x,xs) -> T.toUpper x `T.append` xs) . T.splitAt 1

        score :: Char -> Char
        score '_' = ' '
        score x = x

postNewArticleR :: Handler Html
postNewArticleR = getNewArticleR

getEditArticleR :: Text -> Handler Html
getEditArticleR slug = do
    record <- runDB $ getArticle404 slug

    ((res,form), enctype) <- runFormPost $ articleForm $ Just record

    case res of
        FormSuccess af -> upsertArticle af
        _              -> return ()

    defaultLayout $ do
        setTitle "Edit article"
        $(widgetFile "article/edit")

postEditArticleR :: Text -> Handler Html
postEditArticleR = getEditArticleR

getDelPostR slug = do
    msg <- runDB $ do
        mentity <- getBy $ UniqueArticle slug

        case mentity of
            Just (Entity key _) -> do
                deleteWhere [TagArticle ==. key]
                delete key
                return "Article deleted!"

            _ -> return "Article not found"
    setMessage msg
    redirect ManageArticlesR
    
