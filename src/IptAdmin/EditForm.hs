
module IptAdmin.EditForm where

import Control.Monad.State
import Data.Either
import Data.Maybe
import Data.Set (size)
import IptAdmin.EditForm.Class
import IptAdmin.EditForm.Types
import IptAdmin.Utils
import Iptables
import Iptables.Parser
import Iptables.Types
import Text.ParserCombinators.Parsec.Prim hiding (State (..))

{- | По набору параметров формы получаем набор сообщений об ошибках,
 - либо набор сообщений и правило.
 - Функция возвращает набор сообщений, которые будут показаны в вебинтерфейсе, либо
 - если ошибок нет, то правило + набор сообщений, которые будут показаны в интерфейсе.
 - Сделано так потому, что в интерфейсе нужно отобразить статус проверки в любом случае,
 - даже если введённые данные полностью корректны
 -
 - Кроме этого необходимо отобразить статус проверки для каждого параметра, для этого тип FormError
 - проходит по всем проверкам и остаётся на выходе
 -
 - Сначала выполняем парсинг всех полей ввода,
 - затем для каждой опции выполняем логическую проверку правила
 -}
editPageProcessParams :: EditForm a => a -> Either [ResMessage] ([RuleOption], RuleTarget, [ResMessage])
editPageProcessParams form =
    let entryList = toEntryList form
        -- Парсим список пунктов формы
        parseResList = map parseFormEntry entryList
        {- Для каждого элемента результата парсинга проверяем наличие необходимого модуля iptables,
         - для этого передаём список RuleMatch
         -}
        parseResList' = map (checkMatch $ lefts $ mapMaybe fst parseResList) parseResList
        {- TODO: проверка зависимости действий от параметров. Например, в -xNAT можно указывать порты
         - только в том случае, если есть опция -p udp или -p tcp
         -}

        isThereError :: [(a, ResMessage)] -> Bool
        isThereError resList =
            let errors = filter isError resList

                isError :: (a, ResMessage) -> Bool
                isError (_, RMError _) = True
                isError _ = False
            in not $ null errors
    in
    if isThereError parseResList' then
        Left $ map snd parseResList'
                                 else
        Right $ (,,) (lefts $ mapMaybe fst parseResList')
                     (head $ rights $ mapMaybe fst parseResList')
                     (map snd parseResList')

{- | Обработка параметров формы.
 - На входе параметр формы,
 - на выходе (опция или действие) или ничего и сообщение о результатах обработки параметра формы
 -}
parseFormEntry :: FormEntry -> (Maybe (Either RuleOption RuleTarget), ResMessage)
parseFormEntry entry =
    case entry of
        (FESrc en inv str) ->
            if en
                then
                    let addrE = parse (pWrapper ipAddressParser) "src" str
                    in
                        case addrE of
                            Left _ -> (Nothing, RMError "parsing error")
                            Right addr -> (Just $ Left $ OSource (not inv) addr, RMSucc "")
                else
                    (Nothing, RMIgnore)
        (FEDst en inv str) ->
            if en
                then
                    let addrE = parse (pWrapper ipAddressParser) "dst" str
                    in
                        case addrE of
                            Left _ -> (Nothing, RMError "parsing error")
                            Right addr -> (Just $ Left $ ODest (not inv) addr, RMSucc "")
                else
                    (Nothing, RMIgnore)
        (FEProt en inv prot) ->
            if en
                then
                    case prot of
                        FTCP -> (Just $ Left $ OProtocol (not inv) "tcp", RMSucc "")
                        FUDP -> (Just $ Left $ OProtocol (not inv) "udp", RMSucc "")
                        FICMP -> (Just $ Left $ OProtocol (not inv) "icmp", RMSucc "")
                else
                    (Nothing, RMIgnore)
        (FESPort en inv str) ->
            if en
                then
                    let portE = parse (pWrapper ipPortParser) "source port" str
                    in
                        case portE of
                            Left _ -> (Nothing, RMError "parsing error")
                            Right port -> (Just $ Left $ OSourcePort (not inv) port, RMSucc "")
                 else
                    (Nothing, RMIgnore)
        (FEDPort en inv str) ->
            if en
                then
                    let portE = parse (pWrapper ipPortParser) "dest port" str
                    in
                        case portE of
                            Left _ -> (Nothing, RMError "parsing error")
                            Right port -> (Just $ Left $ ODestPort (not inv) port, RMSucc "")
                else
                    (Nothing, RMIgnore)
        (FEInput en inv str) ->
            if en
                then
                    let interfaceE = parse (pWrapper interfaceParser) "input interface" str
                    in
                        case interfaceE of
                            Left _ -> (Nothing, RMError "parsing error")
                            Right interface -> (Just $ Left $ OInInt (not inv) $ Interface interface, RMSucc "")
                else
                    (Nothing, RMIgnore)
        (FEOutput en inv str) ->
            if en
                then
                    let interfaceE = parse (pWrapper interfaceParser) "output interface" str
                    in
                        case interfaceE of
                            Left _ -> (Nothing, RMError "parsing error")
                            Right interface -> (Just $ Left $ OOutInt (not inv) $ Interface interface, RMSucc "")
                else
                    (Nothing, RMIgnore)
        (FEState en stateSet) ->
            if en
                then
                    if size stateSet == 0
                        then (Nothing, RMError "select states, please")
                        else (Just $ Left $ OState stateSet, RMSucc "")
                else
                    (Nothing, RMIgnore)
        (FEFiltTar ft rejectType userChain) ->
            case ft of
                FAccept -> (Just $ Right TAccept, RMSucc "")
                FDrop -> (Just $ Right TDrop, RMSucc "")
                FReject -> (Just $ Right $ TReject rejectType, RMSucc "")
                FFUserChain -> parseUserChainEntry userChain
        (FENatPrerOutTar ft dnataddr dnatrand dnatpersist redirport redirrand userChain) ->
            case ft of
                FDNat -> parseDNatEntry dnataddr dnatrand dnatpersist
                FRedirect -> parseRedirectEntry redirport redirrand
                FNPrerUserChain -> parseUserChainEntry userChain
        (FENatPostrTar ft snataddr snatrand snatpersist masqport masqrand userChain) ->
            case ft of
                FMasq -> parseMasqEntry masqport masqrand
                FSNat -> parseSNatEntry snataddr snatrand snatpersist
                FNPostrUserChain -> parseUserChainEntry userChain
        (FENatUserTar ft dnatAddr dnatRand dnatPersist redirPort redirRand
                      snatAddr snatRand snatPersist masqPort masqRand userChain) ->
            case ft of
                FUDNat -> parseDNatEntry dnatAddr dnatRand dnatPersist
                FURedirect -> parseRedirectEntry redirPort redirRand
                FUSNat -> parseSNatEntry snatAddr snatRand snatPersist
                FUMasq -> parseMasqEntry masqPort masqRand
                FNUserUserChain -> parseUserChainEntry userChain
        a -> (Nothing, RMError $ "Unknown form entry: " ++ show a)

parseDNatEntry :: String -> Bool -> Bool -> (Maybe (Either RuleOption RuleTarget), ResMessage)
parseDNatEntry dnatAddr dnatRand dnatPersist =
    let dnatAddrE = parse (pWrapper natAddrParser) "dnat address" dnatAddr
    in case dnatAddrE of
        Left _ -> (Nothing, RMError "parsing error")
        Right dnatAddr' -> (Just $ Right $ TDNat dnatAddr' dnatRand dnatPersist, RMSucc "")

parseRedirectEntry :: String -> Bool -> (Maybe (Either RuleOption RuleTarget), ResMessage)
parseRedirectEntry redirPort redirRand =
    let redirPortE = parse (pWrapper natPortParser) "redirect port" redirPort
    in case redirPortE of
        Left _ -> (Nothing, RMError "parsing error")
        Right redirPort' -> (Just $ Right $ TRedirect redirPort' redirRand, RMSucc "")

parseSNatEntry :: String -> Bool -> Bool -> (Maybe (Either RuleOption RuleTarget), ResMessage)
parseSNatEntry snatAddr snatRand snatPersist =
    let snatAddrE = parse (pWrapper natAddrParser) "snat address" snatAddr
    in case snatAddrE of
        Left _ -> (Nothing, RMError "parsing error")
        Right snatAddr' -> (Just $ Right $ TSNat snatAddr' snatRand snatPersist, RMSucc "")

parseMasqEntry :: String -> Bool -> (Maybe (Either RuleOption RuleTarget), ResMessage)
parseMasqEntry masqPort masqRand =
     let masqPortE =
            if masqPort == "" then Right NatPortDefault
                              else parse (pWrapper natPortParser) "masquerade port" masqPort
     in
        case masqPortE of
            Left _ -> (Nothing, RMError "parsing error")
            Right masqPort' -> (Just $ Right $ TMasquerade masqPort' masqRand, RMSucc "")

parseUserChainEntry :: String -> (Maybe (Either RuleOption RuleTarget), ResMessage)
parseUserChainEntry userChain = (Just $ Right $ TUChain userChain, RMSucc "")

{- | Проверка зависимостей опции и таргета от других опций
 - на входе список всех распарсенных опций и результат обработки пункта формы
 -}
checkMatch :: [RuleOption]
           -> (Maybe (Either RuleOption RuleTarget), ResMessage)
           -> (Maybe (Either RuleOption RuleTarget), ResMessage)
checkMatch opts (entry, mes) =
    let isItTcp = not $ null $ filter (OProtocol True "tcp" ==) opts
        isItUdp = not $ null $ filter (OProtocol True "udp" ==) opts
    in
        case entry of
            Just (Left (OSourcePort _ _)) ->
                if isItTcp || isItUdp
                    then (entry, mes)
                    else (Nothing, RMError "please select tcp or udp protocol")
            Just (Left (ODestPort _ _)) ->
                if isItTcp || isItUdp
                    then (entry, mes)
                    else (Nothing, RMError "please select tcp or udp protocol")
            Just (Right (TReject RTTcpReset)) ->
                if isItTcp
                    then (entry, mes)
                    else (Nothing, RMError "the parameter works only with tcp protocol")
            Just (Right (TSNat (NAIpPort _ _ _ _) _ _)) ->
                if isItTcp || isItUdp
                    then (entry, mes)
                    else (Nothing, RMError "please select tcp or udp protocol")
            Just (Right (TDNat (NAIpPort _ _ _ _) _ _)) ->
                if isItTcp || isItUdp
                    then (entry, mes)
                    else (Nothing, RMError "please select tcp or udp protocol")
            Just (Right (TMasquerade (NatPort _ _) _)) ->
                if isItTcp || isItUdp
                    then (entry, mes)
                    else (Nothing, RMError "please select tcp or udp protocol")
            Just (Right (TRedirect (NatPort _ _) _)) ->
                if isItTcp || isItUdp
                    then (entry, mes)
                    else (Nothing, RMError "please select tcp or udp protocol")
            a -> (a, mes)

{- | Для каждой опции получает список модулей, от которых она зависит
 - затем для каждого модуля проверяет, есть ли он уже в правиле
 - и если его там нет, то добавляет
 - В состоянии передаётся список опций правила
 - вызывать так: options' <- runState (mapM_ completeModules options) options
 -}
completeModules :: RuleOption -> State [RuleOption] ()
completeModules opt = do
    let depends = optionDepends opt
    mapM_ addModule depends

    where
        addModule :: Module -> State [RuleOption] ()
        addModule m = do
            matches <- get
            let mod' = filter (isModule m) matches
            when (null mod') $
                put $ OModule m : matches

        isModule :: Module -> RuleOption -> Bool
        isModule m ro | OModule m == ro = True
                      | otherwise = False
