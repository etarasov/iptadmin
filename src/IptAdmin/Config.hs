
module IptAdmin.Config where

import Control.Monad.Error
import Data.ConfigFile
import IptAdmin.Types
import System.FilePath.Posix

cONFpATHd :: String
cONFpATHd = "/etc/iptadmin"

cONFIGURATIONf :: String
cONFIGURATIONf = "iptadmin.conf"

getConfig :: ErrorT String IO IptAdminConfig
getConfig = do
    configE <- liftIO $ runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP (cONFpATHd </> cONFIGURATIONf)
        saveCommand <- get cp "DEFAULT" "save command"
        port <- get cp "DEFAULT" "port"
        pamName <- get cp "DEFAULT" "pam name"
        return $ IptAdminConfig saveCommand
                               port
                               pamName
    case configE of
        Left (_, err) -> throwError ("Error on reading " ++ cONFpATHd </> cONFIGURATIONf ++ "\n" ++ err)
        Right config -> return config
