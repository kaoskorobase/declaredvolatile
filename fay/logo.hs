{-# LANGUAGE NoImplicitPrelude #-}
module Logo where

import           Ref
import           Language.Fay.FFI
import           Language.Fay.Prelude

type Html = String

data Pixel = Transparent | Pixel (Char -> Html) !Char

instance Foreign Pixel

pixel :: (Char -> Html) -> Char -> Pixel
pixel _ ' ' = Transparent
pixel f c   = Pixel f c

isTransparent :: Pixel -> Bool
isTransparent Transparent = True
isTransparent _ = False

renderPixel :: Pixel -> Html
renderPixel Transparent = " "
renderPixel (Pixel f c) = f c

type Image = [[Pixel]]

image :: (Char -> Html) -> [[Char]] -> Image
image f = map (map (pixel f))

pre :: Html -> Html
pre h = "<pre>" ++ h ++ "</pre>"

renderImage :: Image -> Html
renderImage =
    pre
  . intercalate "\n"
  . map (concat . map renderPixel)

--prop_identity1 = unlines image == (unlines $ bits `above` sine `above` axes)
--prop_identity2 = unlines image == (unlines $ axes `below` sine `below` bits)

--transparent :: Char
--transparent = ' '

combine :: [Pixel] -> [Pixel] -> [Pixel]
combine as [] = as
combine [] bs = bs
combine (a:as) (b:bs)
  | isTransparent a = b : combine as bs
  | otherwise = a : combine as bs

above :: Image -> Image -> Image
above as [] = as
above [] bs = bs
above (a:as) (b:bs) = combine a b : above as bs

below :: Image -> Image -> Image
below a b = above b a

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n-1) xs

rotateLeftBy :: Int -> Image -> Image
rotateLeftBy n = map f
  where f xs = let m = length xs
                   xs' = take n xs
               in drop (m-n) xs ++ take (m-n) xs

rotateRightBy :: Int -> Image -> Image
rotateRightBy n = map f
  where f xs = let xs' = take n xs in drop n xs ++ xs'

documentWrite :: String -> Fay ()
documentWrite = ffi "document.write(%1)"

data Element
instance Foreign Element
instance Show (Element)

documentGetElementById :: String -> Fay Element
documentGetElementById = ffi "document['getElementById'](%1)"

elementGetId :: Element -> Fay String
elementGetId = ffi "%1.id"

elementSetInnerHTML :: Element -> String -> Fay ()
elementSetInnerHTML = ffi "%1.innerHTML = %2"

-- | Add an event listener to an element.
windowOnLoad :: Fay () -> Fay ()
windowOnLoad = ffi "window.onload = %1"

setInterval :: Double -> Fay () -> Fay ()
setInterval = ffi "window.setInterval(%2, %1)"

ofClass :: String -> Char -> Html
ofClass c t = "<span class=" ++ c ++ ">" ++ [t] ++ "</span>"

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

axes :: Image
axes = image (ofClass "axis") [
    ""   
  , "    |"
  , "    |"
  , "    |"
  , "    |"
  , "    |"
  , "----+-------------------------------"
  , "    |"
  , "    |"
  , "    |"
  ]

sine :: Image
sine = image (ofClass "curve") [
    ""
  , ""
  , ""
  , ""
  , "      ,-.         "
  , "     /   \\        "
  , "    +     +     ,'"
  , " \\ /      \\   /   "
  , "  -*        `-*   "
  , ""
  ]

noise :: Image
noise = image (ofClass "curve") [
    ""
  , ""
  , ""
  , ""
  , ""
  , "                  .                 "
  , "                   . :'  . -     ..."
  , "                     ,              "
  , ""
  , ""
  ]

bits :: Image
bits = image (ofClass "digit") [
    ""
  , ""
  , ""
  , ""
  , ""
  , ""
  , "                       00 1 11001"
  , ""
  , ""
  , ""
  ]

logo :: Image
logo = axes `below` sine `below` bits

--main :: Fay ()
main = windowOnLoad $ do
  e <- documentGetElementById "logo"
  r <- newRef sine
  setInterval 200 $ do
    i <- readRef r
    elementSetInnerHTML e (renderImage (axes `below` i `below` noise `below` bits))
    writeRef r (rotateLeftBy 1 i)
  --elementSetInnerHTML e $ renderImage logo
