{-# LANGUAGE TemplateHaskell #-}

module IptAdmin.Static where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import Happstack.Server.HTTP.Types
import Happstack.Server.SimpleHTTP
import Template
import IptAdmin.Render
import IptAdmin.ShowPage.Render
import IptAdmin.System
import IptAdmin.Types
import IptAdmin.Utils
import Iptables
import Iptables.Types
import System.Random
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String (renderHtml)

-- TODO: 304

pageHandlers :: IptAdmin Response
pageHandlers = msum [ dirs "js/jquery-1.6.2.min.js" $ returnJs jquery162minjs
                    , dirs "js/jquery-ui-1.8.16.custom.min.js" $ returnJs jqueryUi1816customMinJs
                    , dirs "css/humanity/jquery-ui-1.8.16.custom.css" $ returnCss jqueryUi1816customCss
                    , dirs "css/humanity/images/ui-bg_flat_75_aaaaaa_40x100.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-bg_flat_75_aaaaaa_40x100.png")
                    , dirs "css/humanity/images/ui-bg_glass_100_f5f0e5_1x400.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-bg_glass_100_f5f0e5_1x400.png")
                    , dirs "css/humanity/images/ui-bg_glass_25_cb842e_1x400.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-bg_glass_25_cb842e_1x400.png")
                    , dirs "css/humanity/images/ui-bg_glass_70_ede4d4_1x400.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-bg_glass_70_ede4d4_1x400.png")
                    , dirs "css/humanity/images/ui-bg_highlight-hard_100_f4f0ec_1x100.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-bg_highlight-hard_100_f4f0ec_1x100.png")
                    , dirs "css/humanity/images/ui-bg_highlight-hard_65_fee4bd_1x100.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-bg_highlight-hard_65_fee4bd_1x100.png")
                    , dirs "css/humanity/images/ui-bg_highlight-hard_75_f5f5b5_1x100.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-bg_highlight-hard_75_f5f5b5_1x100.png")
                    , dirs "css/humanity/images/ui-bg_inset-soft_100_f4f0ec_1x100.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-bg_inset-soft_100_f4f0ec_1x100.png")
                    , dirs "css/humanity/images/ui-icons_c47a23_256x240.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-icons_c47a23_256x240.png")
                    , dirs "css/humanity/images/ui-icons_cb672b_256x240.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-icons_cb672b_256x240.png")
                    , dirs "css/humanity/images/ui-icons_f08000_256x240.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-icons_f08000_256x240.png")
                    , dirs "css/humanity/images/ui-icons_f35f07_256x240.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-icons_f35f07_256x240.png")
                    , dirs "css/humanity/images/ui-icons_ff7519_256x240.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-icons_ff7519_256x240.png")
                    , dirs "css/humanity/images/ui-icons_ffffff_256x240.png" $ returnPng $
                        $(embedFile "static/css/humanity/images/ui-icons_ffffff_256x240.png")
                    ]

returnJs :: B.ByteString -> IptAdmin Response
returnJs file = return $ Response { rsCode = 200
                                  , rsHeaders = mkHeaders [("Content-type", "text/javascript; charset=utf8")]
                                  , rsFlags = RsFlags True
                                  , rsBody = BL.pack $ B.unpack file
                                  , rsValidator = Nothing
                                  }

returnCss :: B.ByteString -> IptAdmin Response
returnCss file = return $ Response { rsCode = 200
                                   , rsHeaders = mkHeaders [("Content-type", "text/css; charset=utf8")]
                                   , rsFlags = RsFlags True
                                   , rsBody = BL.pack $ B.unpack file
                                   , rsValidator = Nothing
                                   }

returnPng :: B.ByteString -> IptAdmin Response
returnPng file = return $ Response { rsCode = 200
                                   , rsHeaders = mkHeaders [("Content-type", "image/png; charset=utf8")]
                                   , rsFlags = RsFlags True
                                   , rsBody = BL.pack $ B.unpack file
                                   , rsValidator = Nothing
                                   }

jquery162minjs :: B.ByteString
jquery162minjs = $(embedFile "static/js/jquery-1.6.2.min.js")

jqueryUi1816customMinJs :: B.ByteString
jqueryUi1816customMinJs = $(embedFile "static/js/jquery-ui-1.8.16.custom.min.js")

jqueryUi1816customCss :: B.ByteString
jqueryUi1816customCss = $(embedFile "static/css/humanity/jquery-ui-1.8.16.custom.css")
