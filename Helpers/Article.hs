module Helpers.Article
    ( getNextArticle
    , getArticle404
    , getPreviousArticle
    , upsertArticle
    , articleForm
    , formattedTags
    , UTCTime(..)
    , getCurrentTime
    , humanReadableTime
    , humanReadableTime'
    ) where

import Import
import Prelude (init, head, last)
import Control.Monad (when)
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Time.Format.Human
import Database.Persist.Sql (rawSql)
import Text.Shakespeare.Text (st)
import Yesod.Form.Nic (nicHtmlField)
import qualified Data.Text as T

data ArticleForm = ArticleForm
    {   afSlug      :: Text
    ,   afTitle     :: Text
    ,   afContent   :: Text
    ,   afTags      :: Text
    ,   afDraft     :: Bool
    ,   afWasDraft  :: Bool
    }

articleForm :: Maybe (Article,[Tag]) -> Form ArticleForm
articleForm marticle = renderBootstrap $ ArticleForm
    <$> areq    textField   "Slug"  (fmap (articleSlug  . fst)       marticle)
    <*> areq    textField   "Title" (fmap (articleTitle . fst)       marticle)
    <*> areq    textField "Content" (fmap (articleContent . fst)  marticle)
    <*> areq    textField   "Tags"  (fmap (formatTags . snd)         marticle)
    <*> areq    checkBoxField "Draft?" (Just $ isDraft marticle)
    <*> pure (isDraft marticle)

    where
        formatTags :: [Tag] -> Text
        formatTags = T.intercalate ", " . map tagName

        isDraft :: Maybe (Article,[Tag]) -> Bool
        isDraft = maybe True (articleDraft . fst)

upsertArticle :: ArticleForm -> Handler ()
upsertArticle af = do
    now <- liftIO getCurrentTime

    let article = Article
                    { articleSlug = afSlug af
                    , articleTitle = afTitle af
                    , articleContent = afContent af
                    , articlePosted = now
                    , articleDraft = afDraft af
                    }

    runDB $ do
        result <- insertBy article


        aid <- case result of
            -- Didn't exist, was saved
            Right k -> fmap (const k) $
                lift $ setMessage "article created!"

            -- Existed, let's update it
            Left (Entity k _) -> fmap (const k) $ do
                update k $
                    [ ArticleSlug       =.   afSlug af
                    , ArticleTitle      =.   afTitle   af
                    , ArticleContent    =.   afContent af
                    , ArticleDraft      =.   afDraft af
                    ] ++
                    -- update date if publishing
                    [ ArticlePosted =. now | afWasDraft af, not $ afDraft af ]
                -- Remove existing Tags
                deleteWhere [ TagArticle ==. k ]
                
                lift $ setMessage "Article updated!"

        -- insert the new tags
        mapM_ insertBy (parseTags aid $ afTags af)

    redirect ManageArticlesR

    where
        parseTags :: ArticleId -> Text -> [Tag]
        parseTags aid = map (Tag aid) . filter (not . T.null)
                      . map (T.toLower . T.strip) . T.splitOn ","


getNextArticle :: Article -> DB (Maybe Article)
getNextArticle article = getArticleBy [ ArticleDraft !=. True
                                      , ArticleSlug  !=. articleSlug article
                                      , ArticlePosted <. articlePosted article 
                                      ] [Desc ArticlePosted ]
getPreviousArticle :: Article -> DB (Maybe Article)
getPreviousArticle article = getArticleBy [ ArticleDraft !=. True
                                          , ArticleSlug  !=. articleSlug article

                                          , ArticlePosted >. articlePosted article
                                          ] [Asc ArticlePosted]

getArticleBy :: [Filter Article] -> [SelectOpt Article] -> DB (Maybe Article)
getArticleBy filters sorts = do
    articles <- selectList filters $ sorts ++ [LimitTo 1]
    return $ case articles of
        ((Entity _ a):_) -> Just a
        _                -> Nothing

getArticle404 :: Text -> DB (Article,[Tag])
getArticle404 slug = do
    results <- rawSql [st| SELECT ??, ?? FROM "Article"
                           JOIN "Tag" ON "Tag".article = "Article".id
                           WHERE "Article".slug = ? |] [toPersistValue slug]

    when (null results) $ lift notFound

    return ( entityVal . fst $ head results
           , map (entityVal . snd) results
           )


formattedTags :: ArticleId -> [Tag] -> Text
formattedTags articleId = T.intercalate ", "
                        . map tagName
                        . filter ((== articleId) . tagArticle)