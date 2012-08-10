
module IptAdmin.EditIpForwPage where

import Control.Monad.Error
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.EditIpForwForm.Render
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
    -- 1. Получение текущего значения из /proc/...
    forwState <- getForwardingState
    -- 2. Отрисовка формы
    return $ buildResponse $ renderHtml $ do
        editIpForwForm forwState


pageHandlerPost :: IptAdmin Response
pageHandlerPost = do
    forwStateStr <- getInputString "ipForwState"

    -- 1. Проверка параметра формы.
    newValue <- case forwStateStr of
        "off" -> return False
        "on" -> return True
        a -> throwError $ "Ivalid parameter 'ipForwState': " ++ a

    -- 2. Применяем изменения в /proc/sys/net/ipv4/ip_forward и sysctl.conf
    {-
    if newPolicy == policy
        then -- redir $ "/show?table=" ++ tableName ++ bookmarkForJump chainName Nothing
            return $ buildResponse $ "ok:" ++ show newPolicy
        else do
            tryChange (setPolicy tableName chainName newPolicy)
            -- redir $ "/show?table=" ++ tableName ++ bookmarkForJump chainName Nothing
            return $ buildResponse $ "ok:" ++ show newPolicy
            -}

    setForwardingState newValue

    return $ buildResponse $ "ok:" ++ forwStateStr
