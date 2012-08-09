{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.DelChainPage.Render where

import Data.String
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

delChainForm :: String -> String -> Markup
delChainForm tableName chainName =
    H.div ! A.class_ "editForm" $
    H.form ! A.id "delChainform" ! A.method "post" $ do
        H.input ! A.type_ "hidden" ! A.name "table" ! A.value (fromString tableName)
        H.input ! A.type_ "hidden" ! A.name "chain" ! A.value (fromString chainName)
        "Delete chain  "
        fromString chainName
        "  ?"
        H.br
        H.input ! A.id "submit" ! A.type_ "submit" ! A.value "Delete"
