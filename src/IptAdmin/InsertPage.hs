
module IptAdmin.InsertPage where

import Control.Monad.Error
import Control.Monad.State
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.EditForm
import IptAdmin.EditForm.Render
import IptAdmin.EditForm.Class
import IptAdmin.EditForm.Utils
import IptAdmin.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Iptables.Print
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
    rulePosition <- getInputRead "pos"
    table <- getTable tableName
    checkParams table chainName rulePosition

    (PackedEditForm form) <- nullSelForm tableName chainName
    let userChainNames = getUserChains form chainName table
    return $ buildResponse $ renderHtml $ do
        editFormHtml (tableName, chainName, rulePosition, userChainNames) form Nothing

pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"
    rulePosition <- getInputRead "rulePos"
    table <- getTable tableName
    checkParams table chainName rulePosition

    (PackedEditForm editFormParams) <- httpInputToSelForm tableName chainName
    let userChainNames = getUserChains editFormParams chainName table

    let checkResE = editPageProcessParams editFormParams
    case checkResE of
        Left formResp -> return $ buildResponse $ renderHtml $ do
            editFormHtml (tableName, chainName, rulePosition, userChainNames) editFormParams $ Just formResp
        Right (opts, tar, formResp) -> do
            let (_, opts') = runState (mapM_ completeModules opts) opts
            let rule = Rule (Counters 0 0)opts' tar

            submit <- getInputString "submit"
            case submit of
                "Check" -> return $ buildResponse $ renderHtml (do
                    editFormHtml (tableName, chainName, rulePosition, userChainNames) editFormParams $ Just formResp
                    ) -- ++ printRuleForRun rule
                "Submit" -> do
                    tryChange (insertRule tableName chainName rulePosition rule)
                    -- redir $ "/show?table=" ++ tableName ++ bookmarkForJump chainName (Just rulePosition)
                    return $ buildResponse "ok"
                a -> throwError $ "Invalid value for 'submit' parameter: " ++ a

checkParams :: [Chain] -> String -> Int -> IptAdmin ()
checkParams table chainName rulePosition = do
    let chainMay = getChainByName chainName table
    chain <- maybe (throwError $ "Invalid chain name: " ++ chainName)
                   return
                   chainMay

    when (rulePosition > 1 + length (cRules chain)) $
        throwError $ "Index of insertion too big: " ++ show rulePosition
