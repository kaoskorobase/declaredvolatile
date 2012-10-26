{-# LANGUAGE OverloadedStrings #-}
module AsciiArt where

import qualified Data.List as L
import           Text.Blaze (ToMarkup(..))
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String (renderHtml)

data Pixel = Transparent | Pixel (Char -> Html) !Char

instance ToMarkup Pixel where
  toMarkup Transparent = toMarkup ' '
  toMarkup (Pixel f c) = f c

pixel :: (Char -> Html) -> Char -> Pixel
pixel _ ' ' = Transparent
pixel f c   = Pixel f c

type Layer = [[Pixel]]

layer :: (Char -> Html) -> [[Char]] -> Layer
layer f = fmap (fmap (pixel f))

newtype Image = Image { layers :: [Layer] }

image :: [Layer] -> Image
image = Image

instance ToMarkup Image where
  toMarkup = H.pre
           . H.preEscapedToHtml
           . unlines
           . fmap (renderHtml . H.toHtml . fmap H.toHtml)
           . L.foldl' below []
           . layers

ofClass :: H.AttributeValue -> Char -> Html
ofClass c t = H.span ! A.class_ c $ (H.toHtml t)

imageString = [
    "                                    "   
  , "    |                               "
  , "    |                               "
  , "    |                               "
  , "    | ,-.                           "
  , "    |/   \\        .                 "
  , "----+-----+-----,'-.-:'00.1-11001..."
  , "    |      \\   /     ,              "
  , "    |       `-*                     "
  , "    |                               "
  ]

axes = layer (ofClass "axis") [
    "                                    "   
  , "    |                               "
  , "    |                               "
  , "    |                               "
  , "    |                               "
  , "    |                               "
  , "------------------------------------"
  , "    |                               "
  , "    |                               "
  , "    |                               "
  ]

sine = layer (ofClass "curve") [
    "                                    "
  , "                                    "
  , "                                    "
  , "                                    "
  , "      ,-.                           "
  , "     /   \\        .                 "
  , "    +     +     ,' . :'  . -     ..."
  , "           \\   /     ,              "
  , "            `-*                     "
  , "                                    "
  ]

bits = layer (ofClass "digit") [
    "                                    "
  , "                                    "
  , "                                    "
  , "                                    "
  , "                                    "
  , "                                    "
  , "                       00 1 11001   "
  , "                                    "
  , "                                    "
  , "                                    "
  ]

logo = image [axes, sine, bits]

--prop_identity1 = unlines image == (unlines $ bits `above` sine `above` axes)
--prop_identity2 = unlines image == (unlines $ axes `below` sine `below` bits)

--transparent :: Char
--transparent = ' '

isTransparent :: Pixel -> Bool
isTransparent Transparent = True
isTransparent _ = False

combine :: [Pixel] -> [Pixel] -> [Pixel]
combine as [] = as
combine [] bs = bs
combine (a:as) (b:bs)
  | isTransparent a = b : combine as bs
  | otherwise = a : combine as bs

above :: Layer -> Layer -> Layer
above as [] = as
above [] bs = bs
above (a:as) (b:bs) = combine a b : above as bs

below :: Layer -> Layer -> Layer
below a b = above b a
