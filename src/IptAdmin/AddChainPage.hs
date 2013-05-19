
module IptAdmin.AddChainPage where

import Control.Monad.Error
import Happstack.Server.SimpleHTTP
import IptAdmin.EditChainForm.Parse
import IptAdmin.EditChainForm.Render
import IptAdmin.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Iptables.Types
import Text.ParserCombinators.Parsec.Prim hiding (State (..))
import Text.Blaze.Renderer.Pretty (renderHtml)

pageHandlers :: IptAdmin Response
pageHandlers = msum [ methodSP GET pageHandlerGet
                    , methodSP POST pageHandlerPost
                    ]

pageHandlerGet :: IptAdmin Response
pageHandlerGet = do
    tableName <- getInputNonEmptyString "table"
    return $ buildResponse $ renderHtml $ do
        editChainForm (tableName, "") "" Nothing

pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    tableName <- getInputNonEmptyString "table"
    newChainName <- getInputString "newChainName"
    let newChainNameE = parse parseChainName "chain name" newChainName
    case newChainNameE of
        Left e -> return $ buildResponse $ renderHtml $ do
            editChainForm (tableName, "") newChainName $ Just $ "Parameter error: " ++ show e
        Right newChainName' -> do
            iptables <- getIptables
            table <- case tableName of
                "filter" -> return $ tFilter iptables
                "nat" -> return $ tNat iptables
                "mangle" -> return $ tMangle iptables
                "raw" -> return $ tRaw iptables
                a -> throwError $ "Invalid table parameter: " ++ a
            let checkChainMay = getChainByName newChainName' table
            case checkChainMay of
                Just _ -> return $ buildResponse $ renderHtml $ do
                    editChainForm (tableName, "") newChainName' $ Just "A chain with the same name already exists"
                Nothing -> do
                    submit <- getInputString "submit"
                    case submit of
                        "Check" -> return $ buildResponse $ renderHtml $ do
                            editChainForm (tableName, "") newChainName' $ Just "The name is valid"
                        "Submit" -> do
                            tryChange $ addChain tableName newChainName'
                            -- redir $ "/show?table=" ++ tableName ++ bookmarkForJump newChainName' Nothing
                            return $ buildResponse $ "ok:" ++ newChainName'
                        a -> throwError $ "Invalid value for 'submit' parameter: " ++ a
