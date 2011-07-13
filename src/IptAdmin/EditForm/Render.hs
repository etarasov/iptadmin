{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.EditForm.Render where

import Data.Monoid
import Data.Set (member)
import Data.String
import IptAdmin.EditForm.Class
import IptAdmin.EditForm.Types
import Iptables.Types
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

checkBox :: String -> Bool -> Html
checkBox name on = do
    let chBox = H.input ! A.type_ "checkbox" ! A.name (fromString name) ! A.value "on"
    if on then chBox ! A.checked "checked"
          else chBox

printMesTd :: ResMessage -> Html
printMesTd rm = H.td $ fromString $ case rm of
                        RMError mes -> mes
                        RMSucc mes -> "ok " ++ mes
                        RMIgnore -> "ignored"

maybeListToListMaybe :: Maybe [a] -> [Maybe a]
maybeListToListMaybe (Just list) = map Just list
maybeListToListMaybe Nothing = repeat Nothing

editFormHtml :: EditForm a => (String,String,Int,[String]) -> a -> Maybe [ResMessage] -> Html
editFormHtml (tableName, chainName, rulePos, userChainNames) form errorListMay =
    let entryList = toEntryList form
        mesListMay = maybeListToListMaybe errorListMay
    in
        H.div ! A.class_ "editForm" $
            H.form ! A.id "editform" ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "table" ! A.value (fromString tableName)
                H.input ! A.type_ "hidden" ! A.name "chain" ! A.value (fromString chainName)
                H.input ! A.type_ "hidden" ! A.name "rulePos" ! A.value (fromString $ show rulePos)
                H.table ! A.class_ "editForm" $ do
                    H.tr $ do
                        H.th "Option"
                        H.th "Not"
                        H.th "Parameter"
                        maybe mempty (\_-> H.th "Message") errorListMay
                    mapM_ (renderFormEntry userChainNames) $ zip entryList mesListMay
                H.input ! A.id "check" ! A.name "submit" ! A.type_ "submit" ! A.value "Check"
                H.input ! A.id "submit" ! A.name "submit" ! A.type_ "submit" ! A.value "Submit"

renderFormEntry :: [String] -> (FormEntry, Maybe ResMessage) -> Html
renderFormEntry _ (FESrc en inv str, resMesMay) =
    H.tr $ do
        H.td $ do
            "Source" >> H.br
            checkBox "sourceEnable" en
        H.td $ do
            "!"
            checkBox "sourceInv" inv
        H.td $
            H.input ! A.type_ "text" ! A.name "source" ! A.value (fromString str)
        maybe mempty printMesTd resMesMay
renderFormEntry _ (FEDst en inv str, resMesMay) =
    H.tr $ do
        H.td $ do
            "Destination" >> H.br
            checkBox "destinationEnable" en
        H.td $ do
            "!"
            checkBox "destinationInv" inv
        H.td $
            H.input ! A.type_ "text" ! A.name "destination" ! A.value (fromString str)
        maybe mempty printMesTd resMesMay
renderFormEntry _ (FEProt en inv prot, resMesMay) =
    H.tr $ do
        H.td $ do
            "Protocol" >> H.br
            checkBox "protocolEnable" en
        H.td $ do
            "!"
            checkBox "protocolInv" inv
        H.td $
            H.select ! A.id "protocol" ! A.name "protocol" $ do
                let tcpOpt = H.option ! A.value "tcp" $ "TCP"
                let udpOpt = H.option ! A.value "udp" $ "UDP"
                let icmpOpt = H.option ! A.value "icmp" $ "ICMP"
                case prot of
                    FTCP -> do
                        tcpOpt ! A.selected "selected"
                        udpOpt >> icmpOpt
                    FUDP -> do
                        tcpOpt
                        udpOpt ! A.selected "selected"
                        icmpOpt
                    FICMP -> do
                        tcpOpt >> udpOpt
                        icmpOpt ! A.selected "selected"
        maybe mempty printMesTd resMesMay
renderFormEntry _ (FESPort en inv str, resMesMay) =
    H.tr $ do
        H.td $ do
            "Source port" >> H.br
            checkBox "sportEnable" en
        H.td $ do
            "!"
            checkBox "sportInv" inv
        H.td $
            H.input ! A.type_ "text" ! A.name "sport" ! A.value (fromString str)
        maybe mempty printMesTd resMesMay
renderFormEntry _ (FEDPort en inv str, resMesMay) =
    H.tr $ do
        H.td $ do
            "Destination port" >> H.br
            checkBox "dportEnable" en
        H.td $ do
            "!"
            checkBox "dportInv" inv
        H.td $
            H.input ! A.type_ "text" ! A.name "dport" ! A.value (fromString str)
        maybe mempty printMesTd resMesMay
renderFormEntry _ (FEInput en inv str, resMesMay) =
    H.tr $ do
        H.td $ do
            "Input interface" >> H.br
            checkBox "inputEnable" en
        H.td $ do
            "!"
            checkBox "inputInv" inv
        H.td $
            H.input ! A.type_ "text" ! A.name "input" ! A.value (fromString str)
        maybe mempty printMesTd resMesMay
renderFormEntry _ (FEOutput en inv str, resMesMay) =
    H.tr $ do
        H.td $ do
            "Output interface" >> H.br
            checkBox "outputEnable" en
        H.td $ do
            "!"
            checkBox "outputInv" inv
        H.td $
            H.input ! A.type_ "text" ! A.name "output" ! A.value (fromString str)
        maybe mempty printMesTd resMesMay
renderFormEntry _ (FEState en stateSet, resMesMay) =
    H.tr $ do
        H.td $ do
            "State" >> H.br
            checkBox "stateEnable" en
        H.td ""
        H.td $ do
            checkBox "stateNew" $ CStNew `member` stateSet
            "New" >> H.br
            checkBox "stateEstablished" $ CStEstablished `member` stateSet
            "Established" >> H.br
            checkBox "stateRelated" $ CStRelated `member` stateSet
            "Related" >> H.br
            checkBox "stateInvalid" $ CStInvalid `member` stateSet
            "Invalid" >> H.br
            checkBox "stateUntracked" $ CStUntracked `member` stateSet
            "Untracked" >> H.br
        maybe mempty printMesTd resMesMay

renderFormEntry userChainNames (FEFiltTar filtTar rejectType userChain, resMesMay) =
    H.tr $ do
        H.td ! A.colspan "3" ! A.class_ "inline" $
            H.table ! A.class_ "inline" $ do
                H.tr $ do
                    H.th ! A.class_ "target" $ "Target"
                    H.th ! A.class_ "targetParam" $ "Parameters"
                H.tr $ do
                    H.td ! A.class_ "target" $ do
                        let acceptRadio = H.input ! A.type_ "radio" ! A.name "target" ! A.value "accept"
                        case filtTar of
                            FAccept -> acceptRadio ! A.checked "checked"
                            _ -> acceptRadio
                        "Acccept"
                    H.td ! A.class_ "targetParam" $ ""
                H.tr $ do
                    H.td ! A.class_ "target" $ do
                        let dropRadio = H.input ! A.type_ "radio" ! A.name "target" ! A.value "drop"
                        case filtTar of
                            FDrop -> dropRadio ! A.checked "checked"
                            _ -> dropRadio
                        "Drop"
                    H.td ! A.class_ "targetParam" $ ""
                H.tr $ do
                    H.td ! A.class_ "target" $ do
                        let rejectRadio = H.input ! A.type_ "radio" ! A.name "target" ! A.value "reject"
                        case filtTar of
                            FReject -> rejectRadio ! A.checked "checked"
                            _ -> rejectRadio
                        "Reject" >> H.br
                    H.td ! A.class_ "targetParam" $
                        H.select ! A.id "rejectType" ! A.name "rejectType" $
                            let
                                netUnrOpt = H.option ! A.value "icmp-net-unreachable" $ "Icmp-net-unreachable"
                                netUnrOptSel = if rejectType == RTNetUnreachable then netUnrOpt ! A.selected "selected"
                                                                                 else netUnrOpt
                                hostUnrOpt = H.option ! A.value "icmp-host-unreachable" $ "Icmp-host-unreachable"
                                hostUnrOptSel = if rejectType == RTHostUnreachable then hostUnrOpt ! A.selected "selected"
                                                                                   else hostUnrOpt
                                portUnrOpt = H.option ! A.value "icmp-port-unreachable" $ "Icmp-port-unreachable"
                                portUnrOptSel = if rejectType == RTPortUnreachable then portUnrOpt ! A.selected "selected"
                                                                                  else portUnrOpt
                                protoUnrOpt = H.option ! A.value "icmp-proto-unreachable" $ "Icmp-proto-unreachable"
                                protoUnrOptSel = if rejectType == RTProtoUnreachable then protoUnrOpt ! A.selected "selected"
                                                                                    else protoUnrOpt
                                netProhOpt = H.option ! A.value "icmp-net-prohibited" $ "Icmp-net-prohibited"
                                netProhOptSel = if rejectType == RTNetProhibited then netProhOpt ! A.selected "selected"
                                                                                else netProhOpt
                                hostProhOpt = H.option ! A.value "icmp-host-prohibited" $ "Icmp-host-prohibited"
                                hostProhOptSel = if rejectType == RTHostProhibited then hostProhOpt ! A.selected "selected"
                                                                                  else hostProhOpt
                                admProhOpt = H.option ! A.value "icmp-admin-prohibited" $ "Icmp-admin-prohibited"
                                admProhOptSel = if rejectType == RTAdminProhibited then admProhOpt ! A.selected "selected"
                                                                                  else admProhOpt
                                tcpResetOpt = H.option ! A.value "tcp-reset" $ "Tcp-reset"
                                tcpResetOptSel = if rejectType == RTTcpReset then tcpResetOpt ! A.selected "selected"
                                                                            else tcpResetOpt
                            in
                                netUnrOptSel
                                >> hostUnrOptSel
                                >> portUnrOptSel
                                >> protoUnrOptSel
                                >> netProhOptSel
                                >> hostProhOptSel
                                >> admProhOptSel
                                >> tcpResetOptSel
                if not $ null userChainNames
                    then
                        let checked = case filtTar of
                                FFUserChain -> True
                                _ -> False
                        in renderUserChain checked userChainNames userChain
                    else mempty
        maybe mempty printMesTd resMesMay
renderFormEntry userChainNames (FENatPrerOutTar natPrerTar dnataddr dnatrand dnatpersist redirport redirrand userChain, resMesMay) =
    H.tr $ do
        H.td ! A.colspan "3" ! A.class_ "inline" $
            H.table ! A.class_ "inline" $ do
                H.tr $ do
                    H.th ! A.class_ "target" $ "Target"
                    H.th ! A.class_ "targetParam" $ "Parameters"
                let checked = case natPrerTar of
                        FDNat -> True
                        _ -> False
                renderDNat checked dnataddr dnatrand dnatpersist
                let checked' = case natPrerTar of
                        FRedirect -> True
                        _ -> False
                renderRedirect checked' redirport redirrand
                if not $ null userChainNames
                    then
                        let checked'' = case natPrerTar of
                                FNPrerUserChain -> True
                                _ -> False
                        in renderUserChain checked'' userChainNames userChain
                    else mempty
        maybe mempty printMesTd resMesMay
renderFormEntry userChainNames (FENatPostrTar natPostrTar snataddr snatrand snatpersist masqport masqrand userChain, resMesMay) =
    H.tr $ do
        H.td ! A.colspan "3" ! A.class_ "inline" $
            H.table ! A.class_ "inline" $ do
                H.tr $ do
                    H.th ! A.class_ "target" $ "Target"
                    H.th ! A.class_ "targetParam" $ "Parameters"
                let checked = case natPostrTar of
                        FMasq -> True
                        _ -> False
                renderMasq checked masqport masqrand
                let checked' = case natPostrTar of
                        FSNat -> True
                        _ -> False
                renderSNat checked' snataddr snatrand snatpersist
                if not $ null userChainNames
                    then
                        let checked'' = case natPostrTar of
                                FNPostrUserChain -> True
                                _ -> False
                        in renderUserChain checked'' userChainNames userChain
                    else
                        mempty
        maybe mempty printMesTd resMesMay
renderFormEntry userChainNames (FENatUserTar natUserTar dnatAddr dnatRand dnatPersist redirPort redirRand snatAddr snatRand snatPersist masqPort masqRand userChain, resMesMay) =
    H.tr $ do
        H.td ! A.colspan "3" ! A.class_ "inline" $
            H.table ! A.class_ "inline" $ do
                H.tr $ do
                    H.th ! A.class_ "target" $ "Target"
                    H.th ! A.class_ "targetParam" $ "Parameters"
                let checked = case natUserTar of
                        FUDNat -> True
                        _ -> False
                renderDNat checked dnatAddr dnatRand dnatPersist
                let checked' = case natUserTar of
                        FURedirect -> True
                        _ -> False
                renderRedirect checked' redirPort redirRand
                let checked'' = case natUserTar of
                        FUSNat -> True
                        _ -> False
                renderSNat checked'' snatAddr snatRand snatPersist
                let checked''' = case natUserTar of
                        FUMasq -> True
                        _ -> False
                renderMasq checked''' masqPort masqRand
                if not $ null userChainNames
                    then
                        let checked'''' = case natUserTar of
                                FNUserUserChain -> True
                                _ -> False
                        in renderUserChain checked'''' userChainNames userChain
                    else
                        mempty
        maybe mempty printMesTd resMesMay
renderFormEntry _ a = H.tr $ fromString $ "Unknown form entry: " ++ show a

renderDNat :: Bool -> String -> Bool -> Bool -> Html
renderDNat checked dnatAddr dnatRand dnatPersist =
    H.tr $ do
        H.td ! A.class_ "target" $ do
            let dnatRadio = H.input ! A.type_ "radio" ! A.name "target" ! A.value "dnat"
            if checked then dnatRadio ! A.checked "checked"
                       else dnatRadio
            "Destination Nat"
        H.td ! A.class_ "targetParam" $ do
            "dnat address:"
            H.input ! A.type_ "text" ! A.name "dnataddress" ! A.value (fromString dnatAddr) >> H.br
            "random:"
            checkBox "dnatrandom" dnatRand >> H.br
            "persistent:"
            checkBox "dnatpersistent" dnatPersist >> H.br

renderRedirect :: Bool -> String -> Bool -> Html
renderRedirect checked redirPort redirRand =
    H.tr $ do
        H.td ! A.class_ "target" $ do
            let redirRadio = H.input ! A.type_ "radio" ! A.name "target" ! A.value "redirect"
            if checked then redirRadio ! A.checked "checked"
                       else redirRadio
            "Redirect"
        H.td ! A.class_ "targetParam" $ do
            "port:"
            H.input ! A.type_ "text" ! A.name "redirport" ! A.value (fromString redirPort) >> H.br
            "random:"
            checkBox "redirrandom" redirRand

renderSNat :: Bool -> String -> Bool -> Bool -> Html
renderSNat checked snatAddr snatRand snatPersist =
    H.tr $ do
        H.td ! A.class_ "target" $ do
            let snatRadio = H.input ! A.type_ "radio" ! A.name "target" ! A.value "snat"
            if checked then snatRadio ! A.checked "checked"
                       else snatRadio
            "Source Nat"
        H.td ! A.class_ "targetParam" $ do
            "snat address:"
            H.input ! A.type_ "text" ! A.name "snataddress" ! A.value (fromString snatAddr) >> H.br
            "random:"
            checkBox "snatrandom" snatRand >> H.br
            "persistent:"
            checkBox "snatpersistent" snatPersist >> H.br
renderMasq :: Bool -> String -> Bool -> Html
renderMasq checked masqPort masqRand =
    H.tr $ do
        H.td ! A.class_ "target" $ do
            let masqRadio = H.input ! A.type_ "radio" ! A.name "target" ! A.value "masquerade"
            if checked then masqRadio ! A.checked "checked"
                       else masqRadio
            "Masquerade"
        H.td ! A.class_ "targetParam" $ do
            "port:"
            H.input ! A.type_ "text" ! A.name "masqport" ! A.value (fromString masqPort) >> H.br
            "random:"
            checkBox "masqrandom" masqRand

renderUserChain :: Bool -> [String] -> String -> Html
renderUserChain checked allChains chain =
    H.tr $ do
        H.td ! A.class_ "target" $ do
            let userChainRadio = H.input ! A.type_ "radio" ! A.name "target" ! A.value "userChain"
            if checked then userChainRadio ! A.checked "checked"
                       else userChainRadio
            "User chain"
        H.td ! A.class_ "targetParam" $
            H.select ! A.id "userChain" ! A.name "userChain" $
                mapM_ (renderOption chain) allChains
    where
    renderOption :: String -> String -> Html
    renderOption selName optName = do
        let opt = H.option ! A.value (fromString optName) $ fromString optName
        let optSel = if optName == selName then opt ! A.selected "selected"
                                           else opt
        optSel
