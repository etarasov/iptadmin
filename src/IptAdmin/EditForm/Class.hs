{-# LANGUAGE ExistentialQuantification #-}

module IptAdmin.EditForm.Class where

import Control.Applicative hiding (empty)
import Control.Monad.Error
import Data.List (sort)
import Data.Set (empty, singleton, union)
import IptAdmin.EditForm.Types
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Iptables.Parser
import Iptables.Print
import Iptables.Types
import Text.ParserCombinators.Parsec.Prim hiding (State(..))

data PackedEditForm = forall a . (EditForm a) => PackedEditForm a

class EditForm a where
    toEntryList :: a -> [FormEntry]
    nullForm :: a
    httpInputToForm :: IptAdmin a
    ruleToFormParams :: Rule -> IptAdmin a
    -- Get valid user chains for using as targets
    getUserChains :: a
                  -> String      -- ^ chain name
                  -> [Chain]     -- ^ table
                  -> [String]    -- ^ valid chain names list


instance EditForm FiltInpForm where
    toEntryList ef =
        commParamsToList True False (fifCommonPars ef)
        ++ [FEFiltTar (fifTarget ef)
                      (fifRejectType ef)
                      (fifUserChain ef)
           ]

    nullForm = FiltInpForm nullComPars FAccept RTPortUnreachable ""

    httpInputToForm =
        FiltInpForm <$> httpInputToComParams True False
                    <*> httpInputToFiltTar
                    <*> httpInputToRejectType
                    <*> httpInputToUserChain (nullForm :: FiltInpForm)

    ruleToFormParams rule = do
        comPars <- foldM optToFormParam nullComPars (rOptions rule)
        (tarParam, rejectType, userChain) <- filtTarToParam $ rTarget rule
        return $ FiltInpForm comPars tarParam rejectType userChain

    getUserChains _ chainName chains =
        let userChains = filter (not . isFilterBuiltinChain) $ map cName chains
            res = filter (isFilterType FilterValidChain chains) userChains
            res' = filter (not . findChainForward chainName chains) res
        in sort res'

instance EditForm FiltForwForm where
    toEntryList ef =
        commParamsToList True True (fffCommonPars ef)
        ++ [FEFiltTar (fffTarget ef)
                      (fffRejectType ef)
                      (fffUserChain ef)
           ]

    nullForm = FiltForwForm nullComPars FAccept RTPortUnreachable ""

    httpInputToForm =
        FiltForwForm <$> httpInputToComParams True True
                     <*> httpInputToFiltTar
                     <*> httpInputToRejectType
                     <*> httpInputToUserChain (nullForm :: FiltForwForm)

    ruleToFormParams rule = do
        comPars <- foldM optToFormParam nullComPars (rOptions rule)
        (tarParam, rejectType, userChain) <- filtTarToParam $ rTarget rule
        return $ FiltForwForm comPars tarParam rejectType userChain

    getUserChains _ chainName chains =
        let userChains = filter (not . isFilterBuiltinChain) $ map cName chains
            res = filter (isFilterType FilterValidChain chains) userChains
            res' = filter (not . findChainForward chainName chains) res
        in sort res'

instance EditForm FiltOutForm where
    toEntryList ef =
        commParamsToList False True (fofCommonPars ef)
        ++ [FEFiltTar (fofTarget ef)
                      (fofRejectType ef)
                      (fofUserChain ef)
           ]

    nullForm = FiltOutForm nullComPars FAccept RTPortUnreachable ""

    httpInputToForm =
        FiltOutForm <$> httpInputToComParams False True
                    <*> httpInputToFiltTar
                    <*> httpInputToRejectType
                    <*> httpInputToUserChain (nullForm :: FiltOutForm)

    ruleToFormParams rule = do
        comPars <- foldM optToFormParam nullComPars (rOptions rule)
        (tarParam, rejectType, userChain) <- filtTarToParam $ rTarget rule
        return $ FiltOutForm comPars tarParam rejectType userChain

    getUserChains _ chainName chains =
        let userChains = filter (not . isFilterBuiltinChain) $ map cName chains
            res = filter (isFilterType FilterValidChain chains) userChains
            res' = filter (not . findChainForward chainName chains) res
        in sort res'

instance EditForm NatPrerForm where
    toEntryList ef =
        commParamsToList True False (nrfCommonPars ef)
        ++ [ FENatPrerOutTar (nrfTarget ef)
                             (nrfDNatAddr ef)
                             (nrfDNatRand ef)
                             (nrfDNatPersist ef)
                             (nrfRedirPort ef)
                             (nrfRedirRand ef)
                             (nrfUserChain ef)
           ]

    nullForm = NatPrerForm nullComPars FDNat "" False False "" False ""

    httpInputToForm =
        NatPrerForm <$> httpInputToComParams True False
                    <*> httpInputToNatPrerOutTar
                    <*> getInputString "dnataddress"
                    <*> isThereInput "dnatrandom"
                    <*> isThereInput "dnatpersistent"
                    <*> getInputString "redirport"
                    <*> isThereInput "redirrandom"
                    <*> httpInputToUserChain (nullForm :: NatPrerForm)

    ruleToFormParams rule = do
        comPars <- foldM optToFormParam nullComPars (rOptions rule)
        (tarParam, dnataddr, dnatrand, dnatpersist, redirport, redirrand, userChain) <- natPrerOutTarToParam $ rTarget rule
        return $ NatPrerForm comPars tarParam dnataddr dnatrand dnatpersist redirport redirrand userChain

    getUserChains _ chainName chains =
        let userChains = filter (not . isNatBuiltinChain) $ map cName chains
            res = filter (isNatType NatDNatChain chains) userChains
                ++ filter (isNatType NatUnknownChain chains) userChains
            res' = filter (not . findChainForward chainName chains) res
        in sort res'

instance EditForm NatPostrForm where
    toEntryList ef =
        commParamsToList False True (npfCommonPars ef)
        ++ [ FENatPostrTar (npfTarget ef)
                           (npfSNatAddr ef)
                           (npfSNatRand ef)
                           (npfSNatPersist ef)
                           (npfMasqPort ef)
                           (npfMasqRand ef)
                           (npfUserChain ef)
           ]

    nullForm = NatPostrForm nullComPars FMasq "" False False "" False ""

    httpInputToForm =
        NatPostrForm <$> httpInputToComParams False True
                     <*> httpInputToNatPostrTar
                     <*> getInputString "snataddress"
                     <*> isThereInput "snatrandom"
                     <*> isThereInput "snatpersistent"
                     <*> getInputString "masqport"
                     <*> isThereInput "masqrandom"
                     <*> httpInputToUserChain (nullForm :: NatPostrForm)

    ruleToFormParams rule = do
        comPars <- foldM optToFormParam nullComPars (rOptions rule)
        (tarParam, snataddr, snatrand, snatpersist, masqport, masqrand, userChain) <- natPostrTarToParam $ rTarget rule
        return $ NatPostrForm comPars tarParam snataddr snatrand snatpersist masqport masqrand userChain

    getUserChains _ chainName chains =
        let userChains = filter (not . isNatBuiltinChain) $ map cName chains
            res = filter (isNatType NatSNatChain chains) userChains
                  ++ filter (isNatType NatUnknownChain chains) userChains
            res' = filter (not . findChainForward chainName chains) res
        in sort res'

instance EditForm NatOutForm where
    toEntryList ef =
        commParamsToList False True (nofCommonPars ef)
        ++ [ FENatPrerOutTar (nofTarget ef)
                             (nofDNatAddr ef)
                             (nofDNatRand ef)
                             (nofDNatPersist ef)
                             (nofRedirPort ef)
                             (nofRedirRand ef)
                             (nofUserChain ef)
           ]

    nullForm = NatOutForm nullComPars FDNat "" False False "" False ""

    httpInputToForm =
        NatOutForm <$> httpInputToComParams False True
                   <*> httpInputToNatPrerOutTar
                   <*> getInputString "dnataddress"
                   <*> isThereInput "dnatrandom"
                   <*> isThereInput "dnatpersistent"
                   <*> getInputString "redirport"
                   <*> isThereInput "redirrandom"
                   <*> httpInputToUserChain (nullForm :: NatOutForm)

    ruleToFormParams rule = do
        comPars <- foldM optToFormParam nullComPars (rOptions rule)
        (tarParam, dnataddr, dnatrand, dnatpersist, redirport, redirrand, userChain) <- natPrerOutTarToParam $ rTarget rule
        return $ NatOutForm comPars tarParam dnataddr dnatrand dnatpersist redirport redirrand userChain

    getUserChains _ chainName chains =
        let userChains = filter (not . isNatBuiltinChain) $ map cName chains
            res = filter (isNatType NatDNatChain chains) userChains
                  ++ filter (isNatType NatUnknownChain chains) userChains
            res' = filter (not . findChainForward chainName chains) res
        in sort res'

instance EditForm NatUserForm where
    toEntryList ef =
        commParamsToList True True (nufCommonPars ef)
        ++ [ FENatUserTar (nufTarget ef)
                          (nufDNatAddr ef)
                          (nufDNatRand ef)
                          (nufDNatPersist ef)
                          (nufRedirPort ef)
                          (nufRedirRand ef)
                          (nufSNatAddr ef)
                          (nufSNatRand ef)
                          (nufSNatPersist ef)
                          (nufMasqPort ef)
                          (nufMasqRand ef)
                          (nufUserChain ef)
           ]

    nullForm = NatUserForm nullComPars FUMasq "" False False "" False "" False False "" False ""

    httpInputToForm =
        NatUserForm <$> httpInputToComParams True True
                    <*> httpInputToNatUserTar
                    <*> getInputString "dnataddress"
                    <*> isThereInput "dnatrandom"
                    <*> isThereInput "dnatpersistent"
                    <*> getInputString "redirport"
                    <*> isThereInput "redirrandom"
                    <*> getInputString "snataddress"
                    <*> isThereInput "snatrandom"
                    <*> isThereInput "snatpersistent"
                    <*> getInputString "masqport"
                    <*> isThereInput "masqrandom"
                    <*> httpInputToUserChain (nullForm :: NatUserForm)

    ruleToFormParams rule = do
        comPars <- foldM optToFormParam nullComPars (rOptions rule)
        (tarParam, dNatAddr, dNatRand, dNatPersist, redirPort, redirRand,
         sNatAddr, sNatRand, sNatPersist, masqPort, masqRand, userChain) <- natUserTarToParam $ rTarget rule
        return $ NatUserForm comPars tarParam dNatAddr dNatRand dNatPersist redirPort redirRand
                             sNatAddr sNatRand sNatPersist masqPort masqRand userChain

    getUserChains _ chainName chains =
        let userChains = filter (not . isNatBuiltinChain) $ map cName chains
            res = userChains
            res' = filter (not . findChainForward chainName chains) res
        in sort res'

filtTarToParam :: RuleTarget -> IptAdmin (FFiltTar, RejectType, String)
filtTarToParam tar = case tar of
    TAccept -> return (FAccept, RTPortUnreachable, "")
    TDrop -> return (FDrop, RTPortUnreachable, "")
    TReject rt -> return (FReject, rt, "")
    TUChain uc -> return (FFUserChain, RTPortUnreachable, uc)
    a -> throwError $ "Invalid target for filter table: " ++ show a

natPrerOutTarToParam :: RuleTarget -> IptAdmin (FNatPrerOutTar, String, Bool, Bool, String, Bool, String)
natPrerOutTarToParam tar =
    case tar of
        TDNat nataddr rand pers ->
            return (FDNat, printNatAddr nataddr, rand, pers, "", False, "")
        TRedirect natport rand ->
            return (FRedirect, "", False, False, printNatPort natport, rand, "")
        TUChain userChain ->
            return (FNPrerUserChain, "", False, False, "", False, userChain)
        a -> throwError $ "Invalid target for nat PREROUTING chain: " ++ show a

natPostrTarToParam :: RuleTarget -> IptAdmin (FNatPostrTar, String, Bool, Bool, String, Bool, String)
natPostrTarToParam tar =
    case tar of
        TSNat nataddr rand pers ->
            return (FSNat, printNatAddr nataddr, rand, pers, "", False, "")
        TMasquerade natport rand ->
            return (FMasq, "", False, False, printNatPort natport, rand, "")
        TUChain userChain ->
            return (FNPostrUserChain, "", False, False, "", False, userChain)
        a -> throwError $ "Invalid target for nat POSTROUTING chain: " ++ show a

natUserTarToParam :: RuleTarget -> IptAdmin (FNatUserTar, String, Bool, Bool, String, Bool, String, Bool, Bool, String, Bool, String)
natUserTarToParam tar =
    case tar of
        TDNat natAddr rand pers ->
            return (FUDNat, printNatAddr natAddr, rand, pers, "", False, "", False, False, "" , False, "")
        TRedirect natPort rand ->
            return (FURedirect, "", False, False, printNatPort natPort, rand, "", False, False, "", False, "")
        TSNat natAddr rand pers ->
            return (FUSNat, "", False, False, "", False, printNatAddr natAddr, rand, pers, "", False, "")
        TMasquerade natPort rand ->
            return (FUMasq, "", False, False, "", False, "", False, False, printNatPort natPort, rand, "")
        TUChain userChain ->
            return (FNUserUserChain, "", False, False, "", False, "", False, False, "", False, userChain)
        a -> throwError $ "Invalid target for nat user defined chain: " ++ show a

optToFormParam :: CommonFormPars -> RuleOption -> IptAdmin CommonFormPars
optToFormParam cfp opt =
    case opt of
        OProtocol b p -> case p of
                "tcp" -> return cfp { cfpProt = FTCP, cfpProtEn = True, cfpProtInv = not b}
                "udp" -> return cfp { cfpProt = FUDP, cfpProtEn = True, cfpProtInv = not b}
                "icmp" -> return cfp { cfpProt = FICMP, cfpProtEn = True, cfpProtInv = not b}
                a -> throwError $ "unsupported protocol: " ++ show a
        OSource b addr -> return cfp { cfpSrc = printAddress addr, cfpSrcEn = True, cfpSrcInv = not b}
        ODest b addr -> return cfp { cfpDest = printAddress addr
                                   , cfpDestEn = True
                                   , cfpDestInv = not b
                                   }
        OSourcePort b port -> return cfp { cfpSPort = printPort port
                                         , cfpSPortEn = True
                                         , cfpSPortInv = not b
                                         }
        ODestPort b port -> return cfp { cfpDPort = printPort port
                                       , cfpDPortEn = True
                                       , cfpDPortInv = not b
                                       }
        OInInt b int -> return cfp { cfpInput = printInterface int
                                   , cfpInputEn = True
                                   , cfpInputInv = not b
                                   }
        OOutInt b int -> return cfp { cfpOutput = printInterface int
                                    , cfpOutputEn = True
                                    , cfpOutputInv = not b
                                    }
        OState stateSet -> return cfp { cfpStateEn = True
                                      , cfpState = stateSet
                                      }
        OModule _ -> return cfp
        a -> throwError $ "option is not supported yet: " ++ show a

isFiltTarget :: FormEntry -> Bool
isFiltTarget (FEFiltTar _ _ _) = True
isFiltTarget _ = False

isNatPrerOutTarget :: FormEntry -> Bool
isNatPrerOutTarget (FENatPrerOutTar _ _ _ _ _ _ _) = True
isNatPrerOutTarget _ = False

isNatPostrTarget :: FormEntry -> Bool
isNatPostrTarget (FENatPostrTar _ _ _ _ _ _ _) = True
isNatPostrTarget _ = False

isNatUserTarget :: FormEntry -> Bool
isNatUserTarget (FENatUserTar _ _ _ _ _ _ _ _ _ _ _ _) = True
isNatUserTarget _ = False

httpInputToFiltTar :: IptAdmin FFiltTar
httpInputToFiltTar = do
    targetS <- getInputString "target"
    case targetS of
        "accept" -> return FAccept
        "drop" -> return FDrop
        "reject" -> return FReject
        "userChain" -> return FFUserChain
        a -> throwError $ "unsupported 'target' parameter: " ++ show a

httpInputToRejectType :: IptAdmin RejectType
httpInputToRejectType = do
    rtS <- getInputString "rejectType"
    let rtE = parse (pWrapper rejectTypeParser) "rejectType" rtS
    case rtE of
        Left e -> throwError $ "Error while parsing parameter: " ++ show e
        Right rt -> return rt

httpInputToUserChain :: EditForm a => a -> IptAdmin String
httpInputToUserChain form = do
    tableName <- getInputNonEmptyString "table"
    chainName <- getInputNonEmptyString "chain"
    iptables <- getIptables
    table <- case tableName of
        "filter" -> return $ tFilter iptables
        "nat" -> return $ tNat iptables
        "mangle" -> return $ tMangle iptables
        a -> throwError $ "invalid table: " ++ a
    let userChains = getUserChains form chainName table
    if null userChains
        then return ""
        else do
            userChain <- getInputString "userChain"
            if userChain `elem` userChains
                then return userChain
                else throwError $ "invalid user chain: " ++ userChain

httpInputToNatPrerOutTar :: IptAdmin FNatPrerOutTar
httpInputToNatPrerOutTar = do
    targetS <- getInputString "target"
    case targetS of
        "dnat" -> return FDNat
        "redirect" -> return FRedirect
        "userChain" -> return FNPrerUserChain
        a -> throwError $ "Invalid 'target' parameter: " ++ show a

httpInputToNatPostrTar :: IptAdmin FNatPostrTar
httpInputToNatPostrTar = do
    targetS <- getInputString "target"
    case targetS of
        "masquerade" -> return FMasq
        "snat" -> return FSNat
        "userChain" -> return FNPostrUserChain
        a -> throwError $ "Invalid 'target' parameter: " ++ show a

httpInputToNatUserTar :: IptAdmin FNatUserTar
httpInputToNatUserTar = do
    targetS <- getInputString "target"
    case targetS of
        "dnat" -> return FUDNat
        "redirect" -> return FURedirect
        "snat" -> return FUSNat
        "masquerade" -> return FUMasq
        "userChain" -> return FNUserUserChain
        a -> throwError $ "Invalid 'target' parameter: " ++ show a

httpInputToComParams :: Bool -> Bool -> IptAdmin CommonFormPars
httpInputToComParams inDev outDev =
    CommonFormPars <$> isThereInput "sourceEnable"
                   <*> isThereInput "sourceInv"
                   <*> getInputString "source"
                   <*> isThereInput "destinationEnable"
                   <*> isThereInput "destinationInv"
                   <*> getInputString "destination"
                   <*> isThereInput "protocolEnable"
                   <*> isThereInput "protocolInv"
                   <*> ( do
                            protS <- getInputString "protocol"
                            case protS of
                                "tcp" -> return FTCP
                                "udp" -> return FUDP
                                "icmp" -> return FICMP
                                a -> throwError $ "unsupported 'protocol' parameter: " ++ show a
                       )
                   <*> isThereInput "sportEnable"
                   <*> isThereInput "sportInv"
                   <*> getInputString "sport"
                   <*> isThereInput "dportEnable"
                   <*> isThereInput "dportInv"
                   <*> getInputString "dport"
                   <*> isThereInput "inputEnable"
                   <*> isThereInput "inputInv"
                   <*> ( if inDev then getInputString "input"
                                  else return ""
                       )
                   <*> isThereInput "outputEnable"
                   <*> isThereInput "outputInv"
                   <*> ( if outDev then getInputString "output"
                                   else return ""
                       )
                   <*> isThereInput "stateEnable"
                   <*> ( do
                            invalid <- isThereInput "stateInvalid"
                            let invalidSet = if invalid then singleton CStInvalid
                                                        else empty
                            established <- isThereInput "stateEstablished"
                            let establishedSet = if established then singleton CStEstablished
                                                                else empty
                            new <- isThereInput "stateNew"
                            let newSet = if new then singleton CStNew
                                                else empty
                            related <- isThereInput "stateRelated"
                            let relatedSet = if related then singleton CStRelated
                                                        else empty
                            untracked <- isThereInput "stateUntracked"
                            let untrackedSet = if untracked then singleton CStUntracked
                                                            else empty
                            let stateSet = invalidSet
                                         `union` establishedSet
                                         `union` newSet
                                         `union` relatedSet
                                         `union` untrackedSet
                            return stateSet
                       )
