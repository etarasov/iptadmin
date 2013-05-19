{-# LANGUAGE OverloadedStrings #-}

module IptAdmin.Template where

import Text.Blaze
import Text.Blaze.Internal
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

htmlWrapper :: Markup -> Markup
htmlWrapper content = do
    preEscapedString $ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
 \<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    H.html ! A.xmlns "http://www.w3.org/1999/xhtml" $ do
        H.head $ do
            H.title "iptadmin"
            H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=utf-8"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/iptadmin-theme/jquery-ui-1.8.16.custom.css"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/iptadmin.css"
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery-1.6.2.min.js" $ ""
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery-ui-1.8.16.custom.min.js" $ ""
        H.body $ do
            content
