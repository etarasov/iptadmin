{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.Render where

import Data.List
import Data.Monoid (mempty)
import Data.String (fromString)
import Data.Version
import Iptables.Print
import Iptables.Types
import Paths_iptadmin (version)
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

header :: String -> String -> Markup
header tableName pageHeader = do
    let filterB = tableName == "filter"
    let mangleB = tableName == "mangle"
    let natB = tableName == "nat"
    title
    logout
    links filterB natB mangleB
    H.div ! A.class_ "pageHeader" $
        fromString pageHeader

title :: Markup
title =
    H.div ! A.id "title" $ do
        H.a ! A.class_ "title" ! A.href "/" $ "IptAdmin"
        H.span ! A.class_ "version" $
            fromString $ intercalate "." $ map show $ versionBranch version

-- Booleans says which link should be distinguished
links :: Bool -> Bool -> Bool -> Markup
links filt nat mangle =
    let
        filtA = H.a ! A.href "/show?table=filter" $ "Filter"
        natA = H.a ! A.href "/show?table=nat" $ "Nat"
        mangleA = H.a ! A.href "/show?table=mangle" $ "Mangle"
    in
        H.div ! A.id "links" $ do
            H.span $
                if filt then filtA ! A.class_ "activeTableLink"
                        else filtA ! A.class_ "tableLink"
            " "
            H.span $
                if nat then natA ! A.class_ "activeTableLink"
                       else natA ! A.class_ "tableLink"
            " "
            H.span $
                if mangle then mangleA ! A.class_ "activeTableLink"
                          else mangleA ! A.class_ "tableLink"

logout :: Markup
logout =
    H.div ! A.class_ "logout" $
        H.a ! A.href "/logout" $ "Logout"

renderTarget :: RuleTarget -> (Markup, Markup)
renderTarget target = case target of
            TAccept -> (H.span ! A.class_ "acceptTarget" $ "ACCEPT", mempty)
            TDrop -> (H.span ! A.class_ "dropTarget" $ "DROP", mempty)
            TReject rejectType ->
                let target' = H.span ! A.class_ "rejectTarget" $ "REJECT"
                    param = case rejectType of
                        RTNetUnreachable -> "icmp-net-unreachable"
                        RTHostUnreachable -> "icmp-host-unreachable"
                        RTPortUnreachable -> "icmp-port-unreachable"
                        RTProtoUnreachable -> "icmp-proto-unreachable"
                        RTNetProhibited -> "icmp-net-prohibited"
                        RTHostProhibited -> "icmp-host-prohibited"
                        RTAdminProhibited -> "icmp-admin-prohibited"
                        RTTcpReset -> "tcp-reset"
                in
                    (target', param)
            TReturn -> (H.span ! A.class_ "returnTarget" $ "RETURN", mempty)
            TSNat natAddr rand persist ->
                let target' = H.span ! A.class_ "snatTarget" $ "SNAT"
                    param = do
                        fromString (printNatAddr natAddr)
                        " "
                        if rand then "random"
                                else mempty :: Markup
                        " "
                        if persist then "persist"
                                   else mempty :: Markup
                in
                    (target', param)
            TDNat natAddr rand persist ->
                let target' = H.span ! A.class_ "dnatTarget" $ "DNAT"
                    param = do
                        fromString (printNatAddr natAddr)
                        " "
                        if rand then "random"
                                else mempty :: Markup
                        " "
                        if persist then "persist"
                                   else mempty :: Markup
                in
                    (target', param)
            TMasquerade natPort rand ->
                let target' = H.span ! A.class_ "masqueradeTarget" $ "MASQUERADE"
                    param = do
                        case natPort of
                            NatPortDefault -> mempty :: Markup
                            _ -> fromString (printNatPort natPort)
                        if rand then do
                                    " "
                                    "random"
                                else mempty :: Markup
                in
                    (target', param)
            TRedirect natPort rand ->
                let target' = H.span ! A.class_ "redirectTarget" $ "REDIRECT"
                    param = do
                        case natPort of
                            NatPortDefault -> mempty :: Markup
                            _ -> fromString (printNatPort natPort)
                        if rand then do
                                    " "
                                    "random"
                                else mempty :: Markup
                in
                    (target', param)
            TUChain chainName ->
                let target' = H.span ! A.class_ "chainTarget" $ "CHAIN"
                    param = fromString chainName
                in
                    (target', param)
            TUnknown tName params ->
                let target' = H.span ! A.class_ "unknownTarget" $ fromString tName
                    param = fromString $ unwords params
                in (target', param)
