{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.EditPolicyForm.Render where

import Data.Monoid
import Data.String
import Iptables.Types
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

editPolicyForm :: (String, String) -> Policy -> Html
editPolicyForm (tableName, chainName) policy = do
    case policy of
        ACCEPT -> mempty :: Html
        DROP -> mempty :: Html
        a -> fromString ("Unsupported policy type: " ++ show a)
    H.div ! A.class_ "editForm" $
        H.form ! A.id "editPolicyForm" ! A.method "post" $ do
            H.input ! A.type_ "hidden" ! A.name "table" ! A.value (fromString tableName)
            H.input ! A.type_ "hidden" ! A.name "chain" ! A.value (fromString chainName)
            H.table $ do
                H.tr $
                    H.td $ do
                        let acceptRadio = H.input ! A.type_ "radio" ! A.name "policy" ! A.value "accept"
                        case policy of
                            ACCEPT -> acceptRadio ! A.checked "checked"
                            _ -> acceptRadio
                        "Accept"
                H.tr $
                    H.td $ do
                        let dropRadio = H.input ! A.type_ "radio" ! A.name "policy" ! A.value "drop"
                        case policy of
                            DROP -> dropRadio ! A.checked "checked"
                            _ -> dropRadio
                        "Drop"
