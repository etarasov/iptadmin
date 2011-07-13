{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.ShowPage where

import Control.Monad.Error
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.Render
import IptAdmin.ShowPage.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Iptables.Types
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

    case table of
        "" -> showFilter iptables
        "filter" -> showFilter iptables
        "nat" -> showNat iptables
        "mangle" -> showMangle iptables
        "raw" -> throwError "We're sorry, the Raw table is not supported yet"
        a -> throwError $ "Invalid table parameter: " ++ a

showFilter :: Iptables -> IptAdmin Response
showFilter iptables = do
    let filter' = renderTable ("filter", "Filter") $ sortFilterTable $ tFilter iptables
    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header "filter" "Iptables Filter table"
        showPageHtml filter'

showNat :: Iptables -> IptAdmin Response
showNat iptables = do
    let nat = renderTable ("nat", "Nat") $ sortNatTable $ tNat iptables
    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header "nat" "Iptables Nat table"
        showPageHtml nat

showMangle :: Iptables -> IptAdmin Response
showMangle iptables = do
    let mangle = renderTable ("mangle", "Mangle") $ sortMangleTable $ tMangle iptables
    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header "mangle" "Iptables Mangle table. Unfortunately rule editing is not supported for Mangle yet."
        showPageHtml mangle

pageHandlerPost :: IptAdmin Response
pageHandlerPost = undefined

showPageHtml :: Html -> Html
showPageHtml table =
    H.div ! A.id "rules" $
        table
