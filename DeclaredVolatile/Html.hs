module DeclaredVolatile.Html (
  encodeLink
) where

import DeclaredVolatile.Rot13 (rot13)
import Text.Blaze.Html

encodeLink :: String -> String -> Markup
encodeLink name href = preEscapedToHtml $
    "<script>document.write("
      ++ show (map rot13 ("<a href=\"" ++ href ++ "\">" ++ name ++ "</a>"))
      ++ ".replace(/[a-zA-Z]/g,function(c){return String.fromCharCode((c<=\"Z\"?90:122)>=(c=c.charCodeAt(0)+13)?c:c-26);}));"
      ++ "</script>"
