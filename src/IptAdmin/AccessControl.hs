
module IptAdmin.AccessControl where

import Control.Exception (catch, SomeException)
import Control.Monad.Error
import Control.Monad.State
import Data.IORef
import Data.Map
import Data.Maybe
import Happstack.Server.SimpleHTTP
import IptAdmin.LoginPage as LoginPage (pageHandlers)
import IptAdmin.Static as Static
import IptAdmin.Types
import IptAdmin.Utils
import Prelude hiding (catch)
import System.Posix.PAM as PAM
import System.Posix.User

-- verify that client is allowed to access server
authorize :: IORef Sessions -> IptAdminConfig -> IptAdmin Response -> IptAdminAuth Response
authorize sessionsIORef config requestHandler =
    -- allow 'static' dir without authorisation
    (dir "static" Static.pageHandlers) `mplus` do
        clientIdE <- getDataFn $ lookCookieValue "sessionId"
        isAuthorised <- case clientIdE of
            Left _ -> return False
            Right a -> do
                sessions <- liftIO $ readIORef sessionsIORef
                let session = Data.Map.lookup a sessions
                case session of
                    Nothing -> return False
                    Just _ -> return True
        if isAuthorised
            then
                -- Run IptAdmin monad with state
                mapServerPartT (addStateToStack (either undefined id clientIdE, sessionsIORef, config)) requestHandler
            else
                msum [ dir "login" $ LoginPage.pageHandlers (IptAdmin.AccessControl.authenticate $ cPamName config)
                                                            sessionsIORef
                     , redir "/login"
                     ]
    where
        addStateToStack :: (Monad m) => MainState
                           -> UnWebT (ErrorT String (StateT MainState m)) a
                           -> UnWebT (ErrorT String m) a
        addStateToStack mainState statefulAction =
            mapErrorT (addStateToStack' mainState) statefulAction

        addStateToStack' :: (Monad m) => MainState
                         -> StateT MainState m (Either String a)
                         -> m (Either String a)
        addStateToStack' mainState statefulAction =
            evalStateT statefulAction mainState

logout :: IptAdmin Response
logout = do
    (sessionId, sessionsIORef, _) <- lift get
    _ <- liftIO $ atomicModifyIORef sessionsIORef
                                    (\ m -> (delete sessionId m, ()))
    expireCookie "sessionId"
    redir "/"

{- Nothing - authentication is successful
-- or Just error message
-}
authenticate :: String -> String -> String -> IO (Maybe String)
authenticate pamName login password = do
    authRes <- PAM.authenticate pamName login password
    case authRes of
        Left a -> return $ Just $ "Pam message: " ++ pamCodeToMessage a
        Right () ->
            {- The user exists in system and password is correct
             - Next authentication stage: check if it's permitted to work with firewall for the user
             -}
            if login == "root"
                then return Nothing
                else do
                    groupEntryMay <- catch (Just `fmap` getGroupEntryForName "iptadmin")
                                           ((\ _ -> return Nothing) :: (SomeException -> IO (Maybe GroupEntry)))
                    case groupEntryMay of
                        Nothing -> return $ Just "The user is not allowed to access iptables"
                        Just groupEntry ->
                            if login `elem` groupMembers groupEntry
                                then return Nothing
                                else return $ Just "The user is not allowed to access iptables"
