
module IptAdmin.DelPage where

import Control.Monad.Error
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.DelPage.Render
import IptAdmin.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Iptables.Print
import Iptables.Types
import Safe
import Text.Blaze.Renderer.Pretty (renderHtml)

pageHandlers :: IptAdmin Response
pageHandlers = msum [ methodSP GET pageHandlerGet
                    , methodSP POST pageHandlerPost
                    ]

pageHandlerGet :: IptAdmin Response
pageHandlerGet = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"
    rulePosition <- getInputRead "pos"

    (_, rule) <- checkParams tableName chainName rulePosition

    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header tableName $ "Delete rule from '" ++ chainName
                         ++ "' chain in '" ++ tableName
                         ++ "' table in position " ++ show rulePosition
        delPageForm (tableName, chainName, rulePosition) $ printRule (rule, rulePosition)

pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"

    rulePosition <- getInputRead "rulePos"

    _ <- checkParams tableName chainName rulePosition

    tryChange (deleteRule tableName chainName rulePosition)
    redir $ "/show?table=" ++ tableName

checkParams :: String -> String -> Int -> IptAdmin (Chain, Rule)
checkParams tableName chainName rulePosition = do
    table <- getTable tableName

    let chainMay = getChainByName chainName table
    chain <- maybe (throwError $ "Invalid chain name: " ++ chainName)
                   return
                   chainMay

    let ruleMay = cRules chain `atMay` (rulePosition - 1)
    rule <- maybe (throwError $ "Rule index out of range: " ++ show rulePosition)
                  return
                  ruleMay
    return (chain, rule)
