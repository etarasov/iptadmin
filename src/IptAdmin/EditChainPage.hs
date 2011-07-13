
module IptAdmin.EditChainPage where

import Control.Monad.Error
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.EditChainForm.Parse
import IptAdmin.EditChainForm.Render
import IptAdmin.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Text.Blaze.Renderer.Pretty (renderHtml)
import Text.ParserCombinators.Parsec.Prim hiding (State (..))

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
        Just _ -> return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
            header tableName $ "Edit name of '" ++ chainName ++ "' chain in '" ++ tableName ++ "' table"
            editChainForm (tableName, chainName) chainName Nothing

pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"

    newChainName <- getInputString "newChainName"

    let newChainNameE = parse parseChainName "chain name" newChainName
    case newChainNameE of
        Left e -> return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
            header tableName $ "Edit name of '" ++ chainName ++ "' chain in '" ++ tableName ++ "' table"
            editChainForm (tableName, chainName) newChainName $ Just $ "Parameter error: " ++ show e
        Right newChainName' ->
            if chainName == newChainName'
                then return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
                    header tableName $ "Edit name of '" ++ chainName ++ "' chain in '" ++ tableName ++ "' table"
                    editChainForm (tableName, chainName) newChainName' $ Just "The name was not changed"
                else do
                    table <- getTable tableName
                    let checkChainMay = getChainByName newChainName' table
                    case checkChainMay of
                        Just _ -> return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
                            header tableName $ "Edit name of '" ++ chainName ++ "' chain in '" ++ tableName ++ "' table"
                            editChainForm (tableName, chainName) newChainName' $
                                Just "A chain with the same name already exists"
                        Nothing -> do
                            submit <- getInputString "submit"
                            case submit of
                                "Check" -> return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
                                    header tableName $ "Edit name of '" ++ chainName
                                                     ++ "' chain in '" ++ tableName
                                                     ++ "' table"
                                    editChainForm (tableName, chainName) newChainName' $ Just "The name is valid"
                                "Submit" -> do
                                    tryChange $ renameChain tableName chainName newChainName'
                                    redir $ "/show?table=" ++ tableName
                                a -> throwError $ "Invalid value for 'submit' parameter: " ++ a
