{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.DelPage.Render where

import Data.String
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

delPageForm :: (String, String, Int) -> String -> Markup
delPageForm (tableName, chainName, rulePos) rule =
    H.div ! A.class_ "editForm" $
        H.form ! A.id "delform" ! A.method "post" $ do
            H.input ! A.type_ "hidden" ! A.name "table" ! A.value (fromString tableName)
            H.input ! A.type_ "hidden" ! A.name "chain" ! A.value (fromString chainName)
            H.input ! A.type_ "hidden" ! A.name "rulePos" ! A.value (fromString $ show rulePos)
            fromString rule
            H.br
            H.input ! A.id "submit" ! A.type_ "submit" ! A.value "Delete"
