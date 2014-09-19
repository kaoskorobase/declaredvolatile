{-# LANGUAGE OverloadedStrings #-}
module DeclaredVolatile.Logo.AsciiArt (
    Image
  , image
  , width
  , height
  , above
  , below
  , translate
  , crop
  , logo
  , axes
  , sine
  , bits
) where

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

isTransparent :: Pixel a -> Bool
isTransparent Transparent = True
isTransparent _ = False

newtype Image a = Image { pixels :: [[Pixel a]] }
                  deriving (Show)

image :: [[Char]] -> Image Char
image = Image . fmap (fmap pixel)

instance Functor Image where
  fmap f = Image . fmap (fmap (fmap f)) . pixels

instance ToMarkup a => ToMarkup (Image a) where
  toMarkup = H.preEscapedToHtml
           . unlines
           . fmap (renderHtml . H.toHtml . fmap H.toHtml)
           . pixels

width :: Image a -> Int
width = maximum . map length . pixels

height :: Image a -> Int
height = length . pixels

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

crop :: Image a -> Image a
crop = Image . crop_x . crop_y . pixels
  where
    crop_y = L.takeWhile (any (not.isTransparent)) . L.dropWhile (all isTransparent)
    leading = length . L.takeWhile isTransparent
    crop_x xs = let n = foldl (\a -> min a . leading)
                              (maximum . map length $ xs)
                              xs
                in map (drop n) xs

translate :: Int -> Int -> Image a -> Image a
translate xoff yoff = Image . tr_y . tr_x . pixels
  where
    tr_x = map (replicate xoff Transparent ++)
    tr_y = (replicate yoff [] ++)

ofClass :: H.ToMarkup a => H.AttributeValue -> a -> Html
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
