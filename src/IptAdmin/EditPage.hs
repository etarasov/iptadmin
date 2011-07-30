module IptAdmin.EditPage where

import Control.Monad.Error
import Control.Monad.State
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.EditForm
import IptAdmin.EditForm.Class
import IptAdmin.EditForm.Render
import IptAdmin.EditForm.Utils
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

    (table, _, rule) <- checkParams tableName chainName rulePosition

    (PackedEditForm formParams) <- ruleToSelForm rule tableName chainName

    let userChainNames = getUserChains formParams chainName table

    return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
        header tableName $ "Edit rule in '" ++ tableName
                         ++ "' table in '" ++ chainName
                         ++ "' chain in position " ++ show rulePosition
        editFormHtml (tableName, chainName, rulePosition, userChainNames) formParams Nothing

pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"
    rulePosition <- getInputRead "rulePos"

    (table, _, _) <- checkParams tableName chainName rulePosition

    (PackedEditForm addFormParams) <- httpInputToSelForm tableName chainName
    let userChainNames = getUserChains addFormParams chainName table

    let checkResE = editPageProcessParams addFormParams
    case checkResE of
        Left formResp -> return $ buildResponse $ Template.htmlWrapper $ renderHtml $ do
            header tableName $ "Edit rule in '" ++ tableName
                             ++ "' table in '" ++ chainName
                             ++ "' chain in position " ++ show rulePosition
            editFormHtml (tableName, chainName, rulePosition, userChainNames) addFormParams $ Just formResp
        Right (opts, tar, formResp) -> do
            let (_, opts') = runState (mapM_ completeModules opts) opts
            let rule = Rule (Counters 0 0) opts' tar

            submit <- getInputString "submit"
            case submit of
                "Check" -> return $ buildResponse $ Template.htmlWrapper $ renderHtml (do
                    header tableName $ "Edit rule in '" ++ tableName
                                     ++ "' table in '" ++ chainName
                                     ++ "' chain in position " ++ show rulePosition
                    editFormHtml (tableName, chainName, rulePosition, userChainNames) addFormParams $ Just formResp
                    ) ++ printRuleForRun rule
                "Submit" -> do
                    tryChange (replaceRule tableName chainName rulePosition rule)
                    redir $ "/show?table=" ++ tableName
                a -> throwError $ "Invalid value for 'submit' parameter: " ++ a

checkParams :: String -> String -> Int -> IptAdmin ([Chain], Chain, Rule)
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
    return (table, chain, rule)
