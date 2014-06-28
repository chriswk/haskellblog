module Handler.Feed where

import Import

import Helpers.Article
import Yesod.RssFeed

getFeedR :: Handler RepRss
getFeedR = do
    articles' <- runDB $ selectList [ArticleDraft !=. True] [Desc ArticlePosted, LimitTo 10]
    case articles' of
        []  -> notFound
        articles -> feedFromArticles $ map entityVal articles


getFeedTagR :: Text -> Handler RepRss
getFeedTagR tag = do
    articles' <- runDB $ do
        tags <- selectList [TagName ==. tag] []

        let pids = map (tagArticle . entityVal) tags
        articles <- selectList [ArticleDraft !=. True, ArticleId <-. pids] [Desc ArticlePosted]

        return $ map entityVal articles

    case articles' of
        []  -> notFound
        articles -> feedFromArticles articles