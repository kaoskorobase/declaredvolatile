module DeclaredVolatile.Date (date) where

import           Data.Time
import           Data.Time.Format

date :: FormatTime t => String -> t -> String
date = formatTime defaultTimeLocale
