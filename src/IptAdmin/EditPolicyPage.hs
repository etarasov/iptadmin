
module IptAdmin.EditPolicyPage where

import Control.Monad.Error
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.EditPolicyForm.Render
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
    chain <- maybe (throwError $ "Invalid chain name: " ++ chainName) return chainMay

    -- 1. Проверка того, что это встроенная цепочка (политика не PUNDEFINED)
    policy <- case cPolicy chain of
                PUNDEFINED -> throwError $ "Not builtin chain: " ++ show chain
                a -> return a
    -- 2. Отрисовка формы
    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header tableName $ "Change policy for '" ++ chainName ++ "' chain in '" ++ tableName ++ "' table"
        editPolicyForm (tableName, chainName) policy


pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"
    table <- getTable tableName
    let chainMay = getChainByName chainName table
    chain <- maybe (throwError $ "Invalid chain name: " ++ chainName) return chainMay

    -- 1. Проверка того, что это встроенная цепочка (политика не PUNDEFINED)
    policy <- case cPolicy chain of
                PUNDEFINED -> throwError $ "Not builtin chain: " ++ show chain
                a -> return a

    -- 2. Проверка параметра формы.
    newPolicyS <- getInputNonEmptyString "policy"
    newPolicy <- case newPolicyS of
        "accept" -> return ACCEPT
        "drop" -> return DROP
        a -> throwError $ "Ivalid parameter 'policy': " ++ a

    -- 3. Если изменилось, применить изменения
    if newPolicy == policy
        then redir $ "/show?table=" ++ tableName
        else do
            tryChange (setPolicy tableName chainName newPolicy)
            redir $ "/show?table=" ++ tableName
