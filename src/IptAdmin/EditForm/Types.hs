
module IptAdmin.EditForm.Types where

import Data.Set (Set, empty)
import Iptables.Types

data FormEntry = FESrc { feEn :: Bool
                       , feInv :: Bool
                       , feSrc :: String
                       }
               | FEDst { feEn :: Bool
                       , feInv :: Bool
                       , feDst :: String
                       }
               | FEProt { feEn :: Bool
                        , feInv :: Bool
                        , feProt :: FProtocol
                        }
               | FESPort { feEn :: Bool
                         , feInv :: Bool
                         , feSPort :: String
                         }
               | FEDPort { feEn :: Bool
                         , feInv :: Bool
                         , feDPort :: String
                         }
               | FEInput { feEn :: Bool
                         , feInv :: Bool
                         , feInput :: String
                         }
               | FEOutput { feEn :: Bool
                          , feInv :: Bool
                          , feOutput :: String
                          }
               | FEState { feEn :: Bool
                         , feState :: Set CState
                         }
               | FEFiltTar { feFiltTar :: FFiltTar
                           , feRejectType :: RejectType
                           , feUserChain :: String
                           }
               | FENatPrerOutTar { feNatPrerTar :: FNatPrerOutTar
                                 , feDNatAddr :: String
                                 , feDNatRand :: Bool
                                 , feDNatPersist :: Bool
                                 , feRedirPort :: String
                                 , feRedirRand :: Bool
                                 , feUserChain :: String
                                 }
               | FENatPostrTar { feNatPostrTar :: FNatPostrTar
                               , feSNatAddr :: String
                               , feSNatRand :: Bool
                               , feSNatPersist :: Bool
                               , feMasqPort :: String
                               , feMasqRand :: Bool
                               , feUserChain :: String
                               }
               | FENatUserTar { feNatUserTar :: FNatUserTar
                              , feDNatAddr :: String
                              , feDNatRand :: Bool
                              , feDNatPersist :: Bool
                              , feRedirPort :: String
                              , feRedirRand :: Bool
                              , feSNatAddr :: String
                              , feSNatRand :: Bool
                              , feSNatPersist :: Bool
                              , feMasqPort :: String
                              , feMasqRand :: Bool
                              , feUserChain :: String
                              }
               | FEMangTar { feMangTar :: FMangTar
                           , feUserChain :: String
                           }
               deriving (Show, Eq)

data CommonFormPars = CommonFormPars { cfpSrcEn :: Bool
                                     , cfpSrcInv :: Bool -- Парметры *Inv означают "установлена ли галочка !"
                                     , cfpSrc :: String
                                     , cfpDestEn :: Bool
                                     , cfpDestInv :: Bool
                                     , cfpDest :: String
                                     , cfpProtEn :: Bool
                                     , cfpProtInv :: Bool
                                     , cfpProt :: FProtocol
                                     , cfpSPortEn :: Bool
                                     , cfpSPortInv :: Bool
                                     , cfpSPort :: String
                                     , cfpDPortEn :: Bool
                                     , cfpDPortInv :: Bool
                                     , cfpDPort :: String
                                     , cfpInputEn :: Bool
                                     , cfpInputInv :: Bool
                                     , cfpInput :: String
                                     , cfpOutputEn :: Bool
                                     , cfpOutputInv :: Bool
                                     , cfpOutput :: String
                                     -- у state можно не использовать <!>. iptables внутри заменяет отрицание на задание всех остальных состояний.
                                     , cfpStateEn :: Bool
                                     , cfpState :: Set CState
                                     }

nullComPars :: CommonFormPars
nullComPars = CommonFormPars False
                             False
                             ""
                             False
                             False
                             ""
                             False
                             False
                             FTCP
                             False
                             False
                             ""
                             False
                             False
                             ""
                             False
                             False
                             ""
                             False
                             False
                             ""
                             False
                             empty

commParamsToList :: Bool -> Bool -> CommonFormPars -> [FormEntry]
commParamsToList inDev outDev comPar =
        [ FESrc (cfpSrcEn comPar)
                (cfpSrcInv comPar)
                (cfpSrc comPar)
        ]
        ++ [ FEDst (cfpDestEn comPar)
                   (cfpDestInv comPar)
                   (cfpDest comPar)
           ]
        ++ [ FEProt (cfpProtEn comPar)
                    (cfpProtInv comPar)
                    (cfpProt comPar)
           ]
        ++ [ FESPort (cfpSPortEn comPar)
                     (cfpSPortInv comPar)
                     (cfpSPort comPar)
           ]
        ++ [ FEDPort (cfpDPortEn comPar)
                     (cfpDPortInv comPar)
                     (cfpDPort comPar)
           ]
        ++ [ FEInput (cfpInputEn comPar)
                     (cfpInputInv comPar)
                     (cfpInput comPar)
           | inDev ]
        ++ [ FEOutput (cfpOutputEn comPar)
                      (cfpOutputInv comPar)
                      (cfpOutput comPar)
           | outDev ]
        ++ [ FEState (cfpStateEn comPar)
                     (cfpState comPar)
           ]

data FiltInpForm = FiltInpForm { fifCommonPars :: CommonFormPars
                               , fifTarget :: FFiltTar
                               , fifRejectType :: RejectType
                               , fifUserChain :: String
                               }

data FiltForwForm = FiltForwForm { fffCommonPars :: CommonFormPars
                                 , fffTarget :: FFiltTar
                                 , fffRejectType :: RejectType
                                 , fffUserChain :: String
                                 }

data FiltOutForm = FiltOutForm { fofCommonPars :: CommonFormPars
                               , fofTarget :: FFiltTar
                               , fofRejectType :: RejectType
                               , fofUserChain :: String
                               }

data NatPrerForm = NatPrerForm { nrfCommonPars :: CommonFormPars
                               , nrfTarget :: FNatPrerOutTar
                               , nrfDNatAddr :: String
                               , nrfDNatRand :: Bool
                               , nrfDNatPersist :: Bool
                               , nrfRedirPort :: String
                               , nrfRedirRand :: Bool
                               , nrfUserChain :: String
                               }

data NatPostrForm = NatPostrForm { npfCommonPars :: CommonFormPars
                                 , npfTarget :: FNatPostrTar   -- | Radio value
                                 , npfSNatAddr :: String
                                 , npfSNatRand :: Bool
                                 , npfSNatPersist :: Bool
                                 , npfMasqPort :: String
                                 , npfMasqRand :: Bool
                                 , npfUserChain :: String
                                 }

data NatOutForm = NatOutForm { nofCommonPars :: CommonFormPars
                             , nofTarget :: FNatPrerOutTar
                             , nofDNatAddr :: String
                             , nofDNatRand :: Bool
                             , nofDNatPersist :: Bool
                             , nofRedirPort :: String
                             , nofRedirRand :: Bool
                             , nofUserChain :: String
                             }

data NatUserForm = NatUserForm { nufCommonPars :: CommonFormPars
                               , nufTarget :: FNatUserTar
                               , nufDNatAddr :: String
                               , nufDNatRand :: Bool
                               , nufDNatPersist :: Bool
                               , nufRedirPort :: String
                               , nufRedirRand :: Bool
                               , nufSNatAddr :: String
                               , nufSNatRand :: Bool
                               , nufSNatPersist :: Bool
                               , nufMasqPort :: String
                               , nufMasqRand :: Bool
                               , nufUserChain :: String
                               }

data FProtocol = FTCP
               | FUDP
               | FICMP
               deriving (Show, Eq)

data FFiltTar = FAccept
              | FDrop
              | FReject
              | FFUserChain
               deriving (Show, Eq)

data FNatPrerOutTar = FDNat
                    | FRedirect
                    | FNPrerUserChain
                    deriving (Show, Eq)

data FNatPostrTar = FSNat
                  | FMasq
                  | FNPostrUserChain
                  deriving (Show, Eq)

data FNatUserTar = FUDNat
                 | FURedirect
                 | FUSNat
                 | FUMasq
                 | FNUserUserChain
                 deriving (Show, Eq)

data FMangTar = FTTL
              | FTOS
              | FMark
              | FMUserChain
              deriving (Show, Eq)

data ResMessage = RMError String
                | RMSucc String
                | RMIgnore
                deriving (Show, Eq)
