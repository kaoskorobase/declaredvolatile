{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as H

import AsciiArt

index :: Html
index = do
    docTypeHtml $ do
        H.head $ do
            meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
            H.title "declared volatile"
            link ! rel "stylesheet" ! href "reset.css" ! type_ "text/css" ! media "screen"
            link ! rel "stylesheet" ! href "style.css" ! type_ "text/css" ! media "screen"
            meta ! name "description" ! content "Born human, declared volatile"
            meta ! name "keywords" ! content "stefan kersten kaos korobase sex drugs rock'n roll"
            meta ! name "robots" ! content "all"
            link ! rel "index" ! A.title "declared volatile" ! href "/"
        body $ H.div ! A.id "wrapper" $ do
            H.div ! A.id "header" $ do
                H.div ! A.id "logo" $ do
                    h1 "declared"
                    h2 "volatile"
                H.div ! A.id "ascii-logo" $ toHtml logo
            H.div ! A.id "sidebar" $ ul $ do
                li $ script $ preEscapedToHtml ("document.write(\"<n uers=\\\"znvygb:fx@x-ubeam.qr\\\">rznvy</n>\".replace(/[a-zA-Z]/g,function(c){return String.fromCharCode((c<=\"Z\"?90:122)>=(c=c.charCodeAt(0)+13)?c:c-26);}));" :: String)
                li $ a ! href "files/sk.pub.asc" $ "pubkey"
                li $ a ! href "http://twitter.com/kaoskorobase" $ "twitter"
                li $ a ! href "http://www.linkedin.com/pub/stefan-kersten/19/91b/194" $ "linked-in"
                li $ a ! href "https://plus.google.com/116882574218560793137" $ "google+"
                li $ a ! href "https://github.com/kaoskorobase" $ "github"

main :: IO ()
main = BL.putStrLn $ H.renderHtml index
