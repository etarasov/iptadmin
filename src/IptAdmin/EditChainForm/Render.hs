{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.EditChainForm.Render where

import Data.Monoid
import Data.String
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

editChainForm :: (String, String) -> String -> Maybe String -> Html
editChainForm (tableName, chainName) newChainName mesMay =
    H.div ! A.class_ "editForm" $
        H.form ! A.id "editChainForm" ! A.method "post" $ do
            H.input ! A.type_ "hidden" ! A.name "table" ! A.value (fromString tableName)
            H.input ! A.type_ "hidden" ! A.name "chain" ! A.value (fromString chainName)
            H.table $
                H.tr $ do
                    H.td $
                        H.input ! A.type_ "text" ! A.name "newChainName" ! A.value (fromString newChainName) ! A.maxlength "20"
                    case mesMay of
                        Nothing -> mempty :: Html
                        Just mes ->
                            H.td $ fromString mes
