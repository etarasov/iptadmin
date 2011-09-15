{-# LANGUAGE TemplateHaskell #-}

module IptAdmin.Static where

import Control.Monad.Error
import qualified Data.ByteString as B
import Data.FileEmbed
import Happstack.Server.FileServe.BuildingBlocks
import Happstack.Server.Types
import Happstack.Server.SimpleHTTP
import IptAdmin.Types
import System.Time

pageHandlers :: IptAdminAuth Response
pageHandlers = msum [ dirs "css/iptadmin.css" $ returnCss $ $(embedFile "static/css/iptadmin.css")
                    , dirs "js/showpage.js" $ returnJs $ $(embedFile "static/js/showpage.js")
                    , dirs "js/jquery-1.6.2.min.js" $ returnJs jquery162minjs
                    , dirs "js/jquery-ui-1.8.16.custom.min.js" $ returnJs jqueryUi1816customMinJs
                    , dirs "css/iptadmin-theme/jquery-ui-1.8.16.custom.css" $ returnCss jqueryUi1816customCss
                    , dirs "css/iptadmin-theme/images/ui-bg_flat_0_aaaaaa_40x100.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-bg_flat_0_aaaaaa_40x100.png")
                    , dirs "css/iptadmin-theme/images/ui-bg_flat_75_ffffff_40x100.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-bg_flat_75_ffffff_40x100.png")
                    , dirs "css/iptadmin-theme/images/ui-bg_glass_55_fbf9ee_1x400.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-bg_glass_55_fbf9ee_1x400.png")
                    , dirs "css/iptadmin-theme/images/ui-bg_glass_65_ffffff_1x400.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-bg_glass_65_ffffff_1x400.png")
                    , dirs "css/iptadmin-theme/images/ui-bg_glass_75_f0f0f0_1x400.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-bg_glass_75_f0f0f0_1x400.png")
                    , dirs "css/iptadmin-theme/images/ui-bg_glass_75_fafafa_1x400.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-bg_glass_75_fafafa_1x400.png")
                    , dirs "css/iptadmin-theme/images/ui-bg_glass_95_fef1ec_1x400.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-bg_glass_95_fef1ec_1x400.png")
                    , dirs "css/iptadmin-theme/images/ui-bg_highlight-hard_75_d7d7d7_1x100.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-bg_highlight-hard_75_d7d7d7_1x100.png")
                    , dirs "css/iptadmin-theme/images/ui-icons_000000_256x240.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-icons_000000_256x240.png")
                    , dirs "css/iptadmin-theme/images/ui-icons_222222_256x240.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-icons_222222_256x240.png")
                    , dirs "css/iptadmin-theme/images/ui-icons_2e83ff_256x240.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-icons_2e83ff_256x240.png")
                    , dirs "css/iptadmin-theme/images/ui-icons_5b5b5b_256x240.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-icons_5b5b5b_256x240.png")
                    , dirs "css/iptadmin-theme/images/ui-icons_cd0a0a_256x240.png" $ returnPng $
                        $(embedFile "static/css/iptadmin-theme/images/ui-icons_cd0a0a_256x240.png")
                    ]

returnJs :: B.ByteString -> IptAdminAuth Response
returnJs file = do
    request <- askRq
    return $ strictByteStringResponse "text/javascript; charset=utf8"
                                      file
                                      (Just (updateTime, request))
                                      0
                                      (toInteger $ B.length file)

returnCss :: B.ByteString -> IptAdminAuth Response
returnCss file = do
    request <- askRq
    return $ strictByteStringResponse "text/css; charset=utf8"
                                      file
                                      (Just (updateTime, request))
                                      0
                                      (toInteger $ B.length file)

returnPng :: B.ByteString -> IptAdminAuth Response
returnPng file = do
    request <- askRq
    return $ strictByteStringResponse "image/png"
                                      file
                                      (Just (updateTime, request))
                                      0
                                      (toInteger $ B.length file)

updateTime :: CalendarTime
updateTime = CalendarTime 2011
                          September
                          13
                          0
                          0
                          0
                          0
                          Tuesday
                          250
                          "GMT"
                          0
                          False

jquery162minjs :: B.ByteString
jquery162minjs = $(embedFile "static/js/jquery-1.6.2.min.js")

jqueryUi1816customMinJs :: B.ByteString
jqueryUi1816customMinJs = $(embedFile "static/js/jquery-ui-1.8.16.custom.min.js")

jqueryUi1816customCss :: B.ByteString
jqueryUi1816customCss = $(embedFile "static/css/iptadmin-theme/jquery-ui-1.8.16.custom.css")
