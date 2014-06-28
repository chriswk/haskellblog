module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)
import Yesod.Form.Nic (YesodNic)
import Network.Gravatar
import Yesod.RssFeed
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

type DB x = YesodDB App x

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muid    <- maybeAuth
        let mgrav = fmap getGravatar muid
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            rssLink FeedR "rss feed"


            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_myblog_css

            addScript $ StaticR js_jquery_min_js
            addScript $ StaticR js_bootstrap_js

            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
        where
            getGravatar :: Entity User -> String
            getGravatar (Entity _ u) = let email = fromMaybe "" $ userEmail u
                                       in gravatar gravatarOpts email

            gravatarOpts :: GravatarOptions
            gravatarOpts = defaultConfig
                { gSize     = Just $ Size 20
                , gDefault  = Just MM
                }
    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    -- Authorization
    isAuthorized ManageArticlesR    _ = authorizeAdmin
    isAuthorized NewArticleR        _ = authorizeAdmin
    isAuthorized (EditArticleR _)   _ = authorizeAdmin
    isAuthorized (DelArticleR  _)   _ = authorizeAdmin
    isAuthorized UsersR             _ = authorizeAdmin

    isAuthorized _ _ = return Authorized

authorizeAdmin :: HandlerT App IO AuthResult
authorizeAdmin = do
    mu <- maybeAuth

    return $ case mu of
        Just (Entity _ u) ->
            if userAdmin u
                then Authorized
                else Unauthorized "Admin rights required"
        _ -> AuthenticationRequired

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (Entity _ i) -> do
                updateFromAx (credsExtra creds) $ identUser i
                return $ Just $ identUser i

            Nothing -> do
                uid <- insert $ User Nothing Nothing False
                _   <- insert $ Ident (credsIdent creds) uid
                updateFromAx (credsExtra creds) uid
                return $ Just uid
        where
            -- updates username/email with values returned by openid
            -- unless values exist there already
            updateFromAx :: [(Text, Text)] -> UserId -> DB ()
            updateFromAx keys uid = maybe (return ()) go =<< get uid

                where
                    go :: User -> DB ()
                    go u = do
                        case (userName u, lookup "openid.ext1.value.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserName =. parseNick val]
                            _                       -> return ()
                        case (userEmail u, lookup "openid.ext1.value.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserEmail =. val]
                            _                       -> return ()

                    parseNick :: Maybe Text -> Maybe Text
                    parseNick = fmap (T.takeWhile (/= '@'))
    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authOpenId Claimed
                        [  ("openid.ax.mode"         , "fetch_request"                          )
                        ,  ("openid.ax.required"     , "email"                                  )
                        ,  ("openid.ax.type.email"   , "http://schema.openid.net/contact/email" )
                        ,  ("openid.ax.ns.ax"        , "http://openid.net/src/ax/1.0"           )
                        ,  ("openid.ns.ax.required"  , "email"                                  )
                        ,  ("openid.ui.icon"         , "true"                                   )
                        ]
                    ]

    authHttpManager = httpManager

    loginHandler = lift $ defaultLayout $ do
        setTitle "Login"
        $(widgetFile "login")
-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodNic App
-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
