
module IptAdmin.System where

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.State
import IptAdmin.Types
import Iptables.Parser
import Iptables.Print
import Iptables.Types
import System.Exit
import System.IO
import System.Process

-- TODO: change waitForProcess to nonblocking version with timeout

getIptablesSave :: IptAdmin String
getIptablesSave = do
    (_, o, _, h) <- liftIO $ runInteractiveCommand "iptables-save"
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> liftIO $ hGetContents o
        ExitFailure a -> throwError $ show a

iptablesRestore :: String -> IO ()
iptablesRestore iptablesStr = do
    (i, _, _, h) <- runInteractiveCommand "iptables-restore"
    forkIO $ hPutStr i iptablesStr
    _ <- waitForProcess h
    return ()

getIptables :: IptAdmin Iptables
getIptables = do
    iprules <- getIptablesSave
    let rE = parseIptables iprules
    either (throwError . show) return rE

getTable :: String -> IptAdmin [Chain]
getTable tableName = do
    iptables <- getIptables
    case tableName of
        "filter" -> return $ tFilter iptables
        "nat" -> return $ tNat iptables
        "mangle" -> return $ tMangle iptables
        "raw" -> return $ tRaw iptables
        a -> throwError $ "Invalid table parameter: " ++ a

-- | iptables -A
appendRule :: String     -- ^ Table name
           -> String     -- ^ Chain name
           -> Rule       -- ^ Rule
           -> IptAdmin ()
appendRule table chain rule = do
    let rule' = printRuleCanonical rule
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ "iptables -t " ++ table ++ " -A " ++ chain ++ " " ++ rule'
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a

-- | iptables -D
deleteRule :: String     -- ^ Table name
           -> String     -- ^ Chain name
           -> Int        -- ^ Rule position
           -> IptAdmin ()
deleteRule table chain rulePos = do
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ "iptables -t " ++ table
                                                ++ " -D " ++ chain
                                                ++ " " ++ show rulePos
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a

-- | iptables -I
insertRule :: String     -- ^ Table name
           -> String     -- ^ Chain name
           -> Int        -- ^ Rule position
           -> Rule       -- ^ Rule
           -> IptAdmin ()
insertRule table chain rulePos rule = do
    let rule' = printRuleCanonical rule
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ "iptables -t " ++ table
                                                ++ " -I " ++ chain
                                                ++ " " ++ show rulePos
                                                ++ " " ++ rule'
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a

replaceRule :: String    -- ^ Table name
            -> String    -- ^ Chain name
            -> Int       -- ^ Rule position
            -> Rule      -- ^ Rule
            -> IptAdmin ()
replaceRule table chain rulePos rule = do
    let rule' = printRuleCanonical rule
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ "iptables -t " ++ table
                                                ++ " -R " ++ chain
                                                ++ " " ++ show rulePos
                                                ++ " " ++ rule'
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a

addChain :: String     -- ^ Table name
         -> String     -- ^ Chain name
         -> IptAdmin ()
addChain table chainName = do
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ "iptables -t " ++ table
                                                ++ " -N " ++ chainName
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a

renameChain :: String      -- ^ Table name
            -> String      -- ^ Chain name
            -> String      -- ^ New chain name
            -> IptAdmin ()
renameChain table chainName newChainName = do
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ "iptables -t " ++ table
                                                ++ " -E " ++ chainName     -- --rename-chain
                                                ++ " " ++ newChainName
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a

-- | Delete user defined chain
deleteChain :: String      -- ^ Table name
            -> String      -- ^ Chain name
            -> IptAdmin ()
deleteChain tableName chainName = do
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ "iptables -t " ++ tableName
                                                   ++ " -X " ++ chainName  -- --delete-chain
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a

-- | Set policy for builtin chain
setPolicy :: String     -- ^ Table name
          -> String     -- ^ Chain name
          -> Policy     -- ^ New policy
          -> IptAdmin ()
setPolicy tableName chainName policy = do
    policyStr <- case policy of
        ACCEPT -> return "ACCEPT"
        DROP -> return "DROP"
        a -> throwError $ "Invalid policy: " ++ show a
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ "iptables -t " ++ tableName
                                                   ++ " -P " ++ chainName
                                                   ++ " " ++ policyStr
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a ++ "iptables -t " ++ tableName ++ " -P " ++ policyStr

saveIptables :: IptAdmin ()
saveIptables = do
    (_, _, config) <- lift get
    (_, _, e, h) <- liftIO $ runInteractiveCommand $ cSaveCommand config
    ec <- liftIO $ waitForProcess h
    case ec of
        ExitSuccess -> return ()
        ExitFailure a -> do
            err <- liftIO $ hGetContents e
            throwError $ err ++ "\n" ++ show a
