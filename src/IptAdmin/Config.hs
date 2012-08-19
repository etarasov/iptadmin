
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
        sslEnabled <- get cp "DEFAULT" "ssl"
        if sslEnabled then do
            createPair <- get cp "SSL" "create pair if does not exist"
            crtPath <- get cp "SSL" "crt path"
            keyPath <- get cp "SSL" "key path"
            return $ IptAdminConfig saveCommand
                                    port
                                    pamName
                                    $ Just $ SSLConfig createPair
                                                       crtPath
                                                       keyPath
            else
            return $ IptAdminConfig saveCommand
                                    port
                                    pamName
                                    Nothing
    case configE of
        Left (_, err) -> throwError ("Error on reading " ++ cONFpATHd </> cONFIGURATIONf ++ "\n" ++ err)
        Right config -> return config
