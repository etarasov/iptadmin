
module IptAdmin.EditForm.Utils where

import Control.Monad.Error
import IptAdmin.EditForm.Class
import IptAdmin.EditForm.Types
import IptAdmin.System
import IptAdmin.Types
import Iptables.Types
import Iptables

nullSelForm :: String        -- ^ Table name
            -> String        -- ^ Chain name
            -> IptAdmin PackedEditForm
nullSelForm tableName chainName =
    case (tableName, chainName) of
        ("filter", "INPUT") -> return $ PackedEditForm (nullForm :: FiltInpForm)
        ("filter", "FORWARD") -> return $ PackedEditForm (nullForm :: FiltForwForm)
        ("filter", "OUTPUT") -> return $ PackedEditForm (nullForm :: FiltOutForm)
        ("nat", "PREROUTING") -> return $ PackedEditForm (nullForm :: NatPrerForm)
        ("nat", "POSTROUTING") -> return $ PackedEditForm (nullForm :: NatPostrForm)
        ("nat", "OUTPUT") -> return $ PackedEditForm (nullForm :: NatOutForm)
        ("mangle", "PREROUTING") -> undefined
        ("mangle", "INPUT") -> undefined
        ("mangle", "FORWARD") -> undefined
        ("mangle", "OUTPUT") -> undefined
        ("mangle", "POSTROUTING") -> undefined
        ("filter", _) -> return $ PackedEditForm (nullForm :: FiltForwForm)
        ("nat", a) -> do
            iptables <- getIptables
            let chainType = guessNatChainType a $ tNat iptables
            case chainType of
                NatUnknownChain -> return $ PackedEditForm (nullForm :: NatUserForm)
                NatInvalidChain -> return $ PackedEditForm (nullForm :: NatUserForm)
                NatDNatChain -> return $ PackedEditForm (nullForm :: NatPrerForm)
                NatSNatChain -> return $ PackedEditForm (nullForm :: NatPostrForm)
        ("mangle", a) -> undefined a
        (a,_) -> throwError $ "unknown table " ++ a

httpInputToSelForm :: String      -- ^ Table name
                   -> String      -- ^ Chain name
                   -> IptAdmin PackedEditForm
httpInputToSelForm tableName chainName =
    case (tableName, chainName) of
        ("filter", "INPUT") -> fmap PackedEditForm (httpInputToForm :: IptAdmin FiltInpForm)
        ("filter", "FORWARD") -> fmap PackedEditForm (httpInputToForm :: IptAdmin FiltForwForm)
        ("filter", "OUTPUT") -> fmap PackedEditForm (httpInputToForm :: IptAdmin FiltOutForm)
        ("nat", "PREROUTING") -> fmap PackedEditForm (httpInputToForm :: IptAdmin NatPrerForm)
        ("nat", "POSTROUTING") -> fmap PackedEditForm (httpInputToForm :: IptAdmin NatPostrForm)
        ("nat", "OUTPUT") -> fmap PackedEditForm (httpInputToForm :: IptAdmin NatOutForm)
        ("mangle", "PREROUTING") -> undefined
        ("mangle", "INPUT") -> undefined
        ("mangle", "FORWARD") -> undefined
        ("mangle", "OUTPUT") -> undefined
        ("mangle", "POSTROUTING") -> undefined
        ("filter", _) -> fmap PackedEditForm (httpInputToForm :: IptAdmin FiltForwForm)
        ("nat", a) -> do
            iptables <- getIptables
            let chainType = guessNatChainType a $ tNat iptables
            case chainType of
                NatUnknownChain -> fmap PackedEditForm (httpInputToForm :: IptAdmin NatUserForm)
                NatInvalidChain -> fmap PackedEditForm (httpInputToForm :: IptAdmin NatUserForm)
                NatDNatChain -> fmap PackedEditForm (httpInputToForm :: IptAdmin NatPrerForm)
                NatSNatChain -> fmap PackedEditForm (httpInputToForm :: IptAdmin NatPostrForm)
        ("mangle", a) -> undefined a
        (a,_) -> throwError $ "unknown table " ++ a

ruleToSelForm :: Rule        -- ^ Rule to convert
              -> String      -- ^ Table name
              -> String      -- ^ Chain name
              -> IptAdmin PackedEditForm
ruleToSelForm rule tableName chainName =
    case (tableName, chainName) of
        ("filter", "INPUT") -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin FiltInpForm)
        ("filter", "FORWARD") -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin FiltForwForm)
        ("filter", "OUTPUT") -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin FiltOutForm)
        ("nat", "PREROUTING") -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin NatPrerForm)
        ("nat", "POSTROUTING") -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin NatPostrForm)
        ("nat", "OUTPUT") -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin NatOutForm)
        ("mangle", "PREROUTING") -> undefined
        ("mangle", "INPUT") -> undefined
        ("mangle", "FORWARD") -> undefined
        ("mangle", "OUTPUT") -> undefined
        ("mangle", "POSTROUTING") -> undefined
        ("filter", _) -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin FiltForwForm)
        ("nat", a) -> do
            iptables <- getIptables
            let chainType = guessNatChainType a $ tNat iptables
            case chainType of
                    NatUnknownChain -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin NatUserForm)
                    NatInvalidChain -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin NatUserForm)
                    NatDNatChain -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin NatPrerForm)
                    NatSNatChain -> fmap PackedEditForm (ruleToFormParams rule :: IptAdmin NatPostrForm)
        ("mangle", a) -> undefined a
        (a,_) -> throwError $ "unknown table " ++ a
