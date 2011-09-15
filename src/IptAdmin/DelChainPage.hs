
module IptAdmin.DelChainPage where

import Control.Monad.Error
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.DelChainPage.Render
import IptAdmin.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Iptables.Types
import Text.Blaze.Renderer.Pretty (renderHtml)

pageHandlers :: IptAdmin Response
pageHandlers = msum [ methodSP GET pageHandlerGet
                    , methodSP POST pageHandlerPost
                    ]

pageHandlerGet :: IptAdmin Response
pageHandlerGet = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"
    table <- getTable tableName
    let chainMay = getChainByName chainName table

    case chainMay of
        Nothing -> throwError $ "Invalid chain name: " ++ chainName
        Just chain ->
            if not $ null $ cRules chain
                then
                    return $ buildResponse $ Template.htmlWrapper $ renderHtml $
                        header tableName $ "'" ++ chainName
                                         ++ "' chain is not empty. Please, remove all rules before deleting a chain"
                else
                    let linkedChains = scanTableForLink (cName chain) table
                    in
                    if not $ null linkedChains
                        then return $ buildResponse $ Template.htmlWrapper $ renderHtml $
                            header tableName $ "There are links to the '" ++ chainName
                                             ++ "' chain from chains : " ++ show linkedChains
                                             ++ ". Please, remove all links before deleting a chain"
                        else
                            return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
                                header tableName $ "Delete '" ++ chainName
                                                 ++ "' user defined chain from '"
                                                 ++ tableName ++ "' table"
                                delChainForm tableName chainName

pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"
    table <- getTable tableName
    let chainMay = getChainByName chainName table

    case chainMay of
        Nothing -> throwError $ "Ivalid chain name: " ++ chainName
        Just chain ->
            if not $ null $ cRules chain
                then throwError $ "Chain " ++ chainName
                                ++ " is not empty. Please, remove all rules before deleting a chain"
                else
                    let linkedChains = scanTableForLink (cName chain) table
                    in
                    if not $ null linkedChains
                        then throwError $ "There are links to the '"
                                        ++ chainName ++ "' chain from chains : "
                                        ++ show linkedChains
                        else do
                            tryChange $ deleteChain tableName chainName
                            -- redir $ "/show?table=" ++ tableName
                            return $ buildResponse "ok"
