{-# LANGUAGE OverloadedStrings #-}
module AsciiArt where

import qualified Data.List as L
import           Text.Blaze (ToMarkup(..))
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String (renderHtml)

data Pixel a = Transparent | Pixel a
               deriving (Show)

instance Functor Pixel where
  fmap _ Transparent = Transparent
  fmap f (Pixel a)   = Pixel (f a)

space :: Html
space = toMarkup ' '

instance ToMarkup a => ToMarkup (Pixel a) where
  toMarkup Transparent = space
  toMarkup (Pixel a)   = toMarkup a

pixel :: Char -> Pixel Char
pixel ' ' = Transparent
pixel m   = Pixel m

newtype Image a = Image { pixels :: [[Pixel a]] }
                  deriving (Show)

image :: [[Char]] -> Image Char
image = Image . fmap (fmap pixel)

instance Functor Image where
  fmap f = Image . fmap (fmap (fmap f)) . pixels

instance ToMarkup a => ToMarkup (Image a) where
  toMarkup = H.pre
           . H.preEscapedToHtml
           . unlines
           . fmap (renderHtml . H.toHtml . fmap H.toHtml)
           . pixels

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

axes = fmap (ofClass "axis") $ image [
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

sine = fmap (ofClass "curve") $ image [
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

bits = fmap (ofClass "digit") $ image [
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

logo :: Image Html
logo = axes `below` sine `below` bits

--prop_identity1 = unlines image == (unlines $ bits `above` sine `above` axes)
--prop_identity2 = unlines image == (unlines $ axes `below` sine `below` bits)

isTransparent :: Pixel a -> Bool
isTransparent Transparent = True
isTransparent _ = False

combine :: [Pixel a] -> [Pixel a] -> [Pixel a]
combine as [] = as
combine [] bs = bs
combine (a:as) (b:bs)
  | isTransparent a = b : combine as bs
  | otherwise = a : combine as bs

above :: Image a -> Image a -> Image a
above (Image as) (Image bs) = Image $ go as bs
  where go as [] = as
        go [] bs = bs
        go (a:as) (b:bs) = combine a b : go as bs

below :: Image a -> Image a -> Image a
below a b = above b a
