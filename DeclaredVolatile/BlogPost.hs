module DeclaredVolatile.BlogPost (BlogPost(..), formatPostDate, postHtml) where

import           Data.Time
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)
import           Text.Blaze.Html (Html)
import           Text.Pandoc

data BlogPost = BlogPost {
    postTitle :: String
  , postDate :: UTCTime
  , postLastModificationDate :: UTCTime
  , postUrl :: String
  , postBody :: Pandoc
  }

formatPostDate :: BlogPost -> String
formatPostDate = formatTime defaultTimeLocale "%Y-%m-%d" . postDate

postHtml :: BlogPost -> Html
postHtml = writeHtml def { writerReferenceLinks = True
                         , writerHtml5 = True
                         , writerHighlight = True }
         . postBody
