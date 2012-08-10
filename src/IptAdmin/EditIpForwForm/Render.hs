{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.EditIpForwForm.Render where

import Data.Monoid
import Data.String
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

editIpForwForm :: Bool -> Markup
editIpForwForm ipForwState = do
    H.div ! A.class_ "editForm" $
        H.form ! A.id "editIpForwForm" ! A.method "post" $ do
            H.table $ do
                H.tr $
                    H.td $ do
                        let forwOn = H.input ! A.type_ "radio" ! A.name "ipForwState" ! A.value "on"
                        case ipForwState of
                            True -> forwOn ! A.checked "checked"
                            _ -> forwOn
                        "On"
                H.tr $
                    H.td $ do
                        let forwOff = H.input ! A.type_ "radio" ! A.name "ipForwState" ! A.value "off"
                        case ipForwState of
                            False -> forwOff ! A.checked "checked"
                            _ -> forwOff
                        "Off"
