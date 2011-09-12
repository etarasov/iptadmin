module IptAdmin.Utils where

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.State
import Data.IORef
import Data.Map
import Happstack.Server
import IptAdmin.System
import IptAdmin.Types
import Safe
import Text.ParserCombinators.Parsec.Prim hiding (State (..))
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

buildResponse :: String -> Response
buildResponse input = let respPlain = toResponse input
                      in respPlain {rsHeaders = mkHeaders [("Content-type", "text/html; charset=utf8")]}

redir :: Monad m => String -> ServerPartT m Response
redir url = seeOther url (toResponse "")

getInputString :: MonadIO m => String -> ServerPartT (ErrorT String m) String
getInputString input = do
    resE <- getDataFn $ look input
    case resE of
        Left _ -> throwError $ "Parameter not found: " ++ input
        Right res -> return res

getInputNonEmptyString :: String -> IptAdmin String
getInputNonEmptyString input = do
    res <- getInputString input
    if res == ""
        then throwError $ "Parameter should not be empty: " ++ input
        else return res

getInputOrEmptyString :: String -> IptAdmin String
getInputOrEmptyString input = do
    b <- isThereInput input
    if not b
        then
            return ""
        else
            getInputString input

getInputRead :: (Read a) => String -> IptAdmin a
getInputRead input = do
    resS <- getInputString input
    let resM = readMay resS
    case resM of
        Just res -> return res
        Nothing -> throwError $ "Error while parsing parameter: " ++ input

isThereInput :: String -> IptAdmin Bool
isThereInput input = do
    resE <- getDataFn $ look input
    return $ case resE of
                Left _ -> False
                Right _ -> True

-- | Parser wrapper that checks eof
pWrapper :: GenParser Char st a -> GenParser Char st a
pWrapper p = spaces >> p >>= \ res -> spaces >> eof >> return res

-- | Wrapper on firewall alteration,
-- it saves current iptables state
-- for emergency rollback
tryChange :: IptAdmin () -> IptAdmin ()
tryChange action = do
    -- Получаем id сессии
    (sessionId, _, _) <- lift get
    -- Добавляем в сессию iptablesStr
    iptablesStr <- getIptablesSave
    (_, sessionsIORef, _) <- lift get
    liftIO $ atomicModifyIORef sessionsIORef $ \ m ->
        let session = m ! sessionId
            session' = session { backup = Just iptablesStr}
        in
            (insert sessionId session' m, ())
    -- Выполняем изменение файрволла
    action
    -- Форкаем ждущий поток и даём ему IORef
    liftIO $ forkIO $ waitAndRollBack sessionsIORef sessionId
    return ()

waitAndRollBack :: IORef Sessions -> String -> IO ()
waitAndRollBack mainState sessionId = do
    threadDelay $ 30 * 1000000
    -- Проверка состояния
    iptablesStrMay <- atomicModifyIORef mainState $ \ m ->
        let session = m ! sessionId
        in
            case backup session of
                Nothing -> (m, Nothing)
                Just iptablesStr ->
                    let session' = session {backup = Nothing}
                        m' = insert sessionId session' m
                    in
                        (m', Just iptablesStr)
    -- Iptables restore
    case iptablesStrMay of
        Nothing -> return ()
        Just iptablesStr ->
            iptablesRestore iptablesStr

bookmarkForJump :: String         -- ^ Chain name
                -> Maybe Int      -- ^ Rule number
                -> String         -- ^ Bookmark with "#"
bookmarkForJump chainName ruleNumMay =
    case ruleNumMay of
        Nothing ->
            "#chain_" ++ chainName
        Just ruleNum ->
            if ruleNum > 20
                then
                    "#" ++ chainName ++ "_" ++ show (ruleNum - 15)
                else
                    "#chain_" ++ chainName
