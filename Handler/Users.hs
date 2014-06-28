module Handler.Users (getUsersR) where

import Import
import Prelude (head)
import Control.Monad (forM)
import Data.Maybe (fromMaybe)

getUsersR :: Handler Html
getUsersR = do
    records <- runDB $ do
        users <- selectList [] [Asc UserId]
        creds <- selectList [] []

        forM users $ \user -> do
            let uid = entityKey user
            let cred = head $ filter ((== uid) . identUser . entityVal) creds

            return (user,cred)

    defaultLayout $ do
        setTitle "All users"
        $(widgetFile "user/index")
