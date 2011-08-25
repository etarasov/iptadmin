{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.ShowPage where

import Control.Monad.Error
import Control.Monad.State
import Data.IORef
import Data.Map
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.Render
import IptAdmin.ShowPage.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Iptables.Types
import System.Random
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String (renderHtml)

pageHandlers :: IptAdmin Response
pageHandlers = msum [ methodSP GET pageHandlerGet
                    , methodSP POST pageHandlerPost
                    ]

pageHandlerGet :: IptAdmin Response
pageHandlerGet = do
    table <- getInputOrEmptyString "table"

    iptables <- getIptables

    countType <- do
        setCountType <- getInputOrEmptyString "countersType"
        case setCountType of
            "bytes" -> do
                let countTypeCookie = mkCookie "countersType" "Bytes"
                addCookie (60 * 60 * 24 * 365 * 10) countTypeCookie
                return CTBytes
            "packets" -> do
                let countTypeCookie = mkCookie "countersType" "Packets"
                addCookie (60 * 60 * 24 * 365 * 10) countTypeCookie
                return CTPackets
            _ -> do
                countTypeMay <- getDataFn $ lookCookieValue "countersType"
                case countTypeMay of
                    Nothing -> return CTPackets
                    Just "Bytes" -> return CTBytes
                    _ -> return CTPackets

    (sessionId, sessionsIO, _) <- lift get
    sessions <- liftIO $ readIORef sessionsIO
    let session = sessions Data.Map.! sessionId

    iptables' <- if sIptables session /= iptables
        then
            liftIO $ atomicModifyIORef sessionsIO $
                \ sessions' ->
                    ( insert sessionId (session {sIptables = iptables}) sessions'
                    , iptables)
        else
            return $ sIptables session

    case table of
        "" -> showFilter countType iptables iptables'
        "filter" -> showFilter countType iptables iptables'
        "nat" -> showNat countType iptables iptables'
        "mangle" -> showMangle countType iptables iptables'
        "raw" -> throwError "We're sorry, the Raw table is not supported yet"
        a -> throwError $ "Invalid table parameter: " ++ a

maxPacketCounterDiff :: [(Chain, Chain)] -> Integer
maxPacketCounterDiff [] = 0
maxPacketCounterDiff ((x,x'):xs) = max (maxPacketCounterDiff' (x,x')) (maxPacketCounterDiff xs)
    where
        maxPacketCounterDiff' :: (Chain, Chain) -> Integer
        maxPacketCounterDiff' (Chain _ _ (Counters p _) rs, Chain _ _ (Counters p' _) rs') = max (p - p') (maxPacketCounterDiff'' $ zip rs rs')

        maxPacketCounterDiff'' :: [(Rule, Rule)] -> Integer
        maxPacketCounterDiff'' [] = 0
        maxPacketCounterDiff'' ((Rule (Counters p _) _ _, Rule (Counters p' _) _ _):rs) = max (p - p') (maxPacketCounterDiff'' rs)

showFilter :: CountersType -> Iptables -> Iptables -> IptAdmin Response
showFilter countType iptables iptables' = do
    refreshString <- liftIO $ show `fmap` (randomRIO (1,100000) :: IO Int)
    let filter' = renderTable ("filter", "Filter")
                              countType
                              (maxPacketCounterDiff $ zip (sortFilterTable $ tFilter iptables) (sortFilterTable $ tFilter iptables'))
                              refreshString
                              (sortFilterTable $ tFilter iptables)
                              (sortFilterTable $ tFilter iptables')
    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header "filter" "Iptables Filter table"
        showPageHtml filter'

showNat :: CountersType -> Iptables -> Iptables -> IptAdmin Response
showNat countType iptables iptables' = do
    refreshString <- liftIO $ show `fmap` (randomRIO (1,100000) :: IO Int)
    let nat = renderTable ("nat", "Nat")
                          countType
                          (maxPacketCounterDiff $ zip (sortNatTable $ tFilter iptables) (sortNatTable $ tFilter iptables'))
                          refreshString
                          (sortNatTable $ tNat iptables)
                          (sortNatTable $ tNat iptables')
    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header "nat" "Iptables Nat table"
        showPageHtml nat

showMangle :: CountersType -> Iptables -> Iptables -> IptAdmin Response
showMangle countType iptables iptables' = do
    refreshString <- liftIO $ show `fmap` (randomRIO (1,100000) :: IO Int)
    let mangle = renderTable ("mangle", "Mangle")
                             countType
                             (maxPacketCounterDiff $ zip (sortMangleTable $ tFilter iptables) (sortMangleTable $ tFilter iptables'))
                             refreshString
                             (sortMangleTable $ tMangle iptables)
                             (sortMangleTable $ tMangle iptables')
    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header "mangle" "Iptables Mangle table. Rule editing is not supported for Mangle yet."
        showPageHtml mangle

showPageHtml :: Html -> Html
showPageHtml table =
    H.div H.! A.id "rules" $
        table

pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    actionS <- getInputOrEmptyString "action"
    case actionS of
        "reset" -> do
            tableName <- getInputOrEmptyString "table"
            chainName <- getInputString "chain"
            iptables <- getIptables
            (sessionId, sessionsIO, _) <- lift get
            sessions <- liftIO $ readIORef sessionsIO
            let session = sessions Data.Map.! sessionId
            liftIO $ atomicModifyIORef sessionsIO $
                \ sessions' ->
                    ( insert sessionId (session {sIptables = iptables}) sessions'
                    , ())
            redir $ "/show?table=" ++ tableName ++ bookmarkForJump chainName Nothing
        "" -> throwError "'action' parameter is not specified"
        a -> throwError $ "unknown value of 'action' parameter: " ++ a
