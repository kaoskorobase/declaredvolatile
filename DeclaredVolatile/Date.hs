module DeclaredVolatile.Date (date) where

import           Data.Time
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)

date :: FormatTime t => String -> t -> String
date = formatTime defaultTimeLocale
