{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.LoginPage where

import Control.Monad.Error
import Data.IORef
import Data.Map (insert)
import Data.Monoid
import Data.String
import Data.Time
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import System.Random
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String (renderHtml)

pageHandlers :: (String -> String -> IO (Maybe String)) -> IORef Sessions -> IptAdminAuth Response
pageHandlers authenticate sessionsIORef =
    msum [ methodSP GET pageHandlerGet
         , methodSP POST (pageHandlerPost authenticate sessionsIORef)
         ]

pageHandlerGet :: IptAdminAuth Response
pageHandlerGet =
    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        title
        loginForm "" Nothing

pageHandlerPost :: (String -> String -> IO (Maybe String)) -> IORef Sessions -> IptAdminAuth Response
pageHandlerPost authenticate sessionsIORef = do
    login <- getInputString "login"
    password <- getInputString "password"
    when (length login > 30) $
        throwError "Login is too long"
    when (length password > 50) $
        throwError "Password is too long"
    -- Первый этап проверки - проверка пользователя unix
    authRes <- liftIO $ authenticate login password
    case authRes of
        Just errorMsg ->
            return $ buildResponse $ htmlWrapper $ renderHtml $ do
                   title
                   loginForm login $ Just errorMsg
        Nothing -> do
            -- 1. Генерируем рандомный id
            sessionId <- liftIO $ replicateM 50 $ randomRIO ('A', 'z')
            -- 2. Добавляем установку кукиса в заголовок
            -- 2.1. Собариаем cookie
            let sessionIdCookie = mkCookie "sessionId" sessionId
            addCookie (60 * 60 * 24 * 365 * 10) sessionIdCookie
            -- 3. Обновляем состояние
            curTime <- liftIO getCurrentTime
            iptables <- getIptables
            _ <- liftIO $ atomicModifyIORef sessionsIORef
                                            (\ m -> ( insert sessionId (Session curTime
                                                                               Nothing
                                                                               iptables
                                                                       ) m
                                                    , ()
                                                    )
                                            )
            -- 4. Редиректим на /show
            redir "/show"

loginForm :: String -> Maybe String -> Html
loginForm login mesMay =
    H.div ! A.class_ "loginForm" $ do
        H.div ! A.class_ "loginForm2" $
            H.form ! A.id "loginForm" ! A.method "post" $ do
                H.table ! A.class_ "loginForm" $ do
                    H.tr $ do
                        H.td ! A.class_ "loginForm" $
                            H.span ! A.class_ "loginForm" $ "login"
                        H.td ! A.class_ "loginForm" $
                            H.input ! A.type_ "text" ! A.name "login" ! A.value (fromString login) ! A.maxlength "30"
                    H.tr $ do
                        H.td ! A.class_ "loginForm" $
                            H.span ! A.class_ "loginForm" $ "password"
                        H.td ! A.class_ "loginForm" $
                            H.input ! A.type_ "password" ! A.name "password" ! A.value "" ! A.maxlength "50"
                H.input ! A.id "submit" ! A.name "submit" ! A.type_ "submit" ! A.value "Sign in"
        maybe mempty fromString mesMay
