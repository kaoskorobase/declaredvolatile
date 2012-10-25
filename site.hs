import Data.Char

image = [    
    "    |                               "
  , "    |                               "
  , "    |                               "
  , "    | ,-.                           "
  , "    |/   \\        .                 "
  , "----+-----+-----,'-.-:'00.1-11001..."
  , "    |      \\   /     ,              "
  , "    |       `-*                     "
  , "    |                               "
  ]

axes = [ 
    "    |                               "
  , "    |                               "
  , "    |                               "
  , "    |                               "
  , "    |                               "
  , "------------------------------------"
  , "    |                               "
  , "    |                               "
  , "    |                               "
  ]

sine = [
    "                                    "
  , "                                    "
  , "                                    "
  , "      ,-.                           "
  , "     /   \\        .                 "
  , "    +     +     ,' . :'  .       ..."
  , "           \\   /     ,              "
  , "            `-*                     "
  , "                                    "
  ]

bits = [
    "                                    "
  , "                                    "
  , "                                    "
  , "                                    "
  , "                                    "
  , "                       00 1 11001   "
  , "                                    "
  , "                                    "
  , "                                    "
  ]

--SK AT SAMPLECOUNT DOT COM
--GPG
--T W I T T E R
--G00GLE+
--LINKED-IN
--GITHUB
--B1TBuCKeT
--H4SKELL3RS

prop_identity1 = unlines image == (unlines $ bits `above` sine `above` axes)
prop_identity2 = unlines image == (unlines $ axes `below` sine `below` bits)

transparent :: Char
transparent = ' '

isTransparent :: Char -> Bool
isTransparent = (==transparent)

combine :: String -> String -> String
combine as [] = as
combine [] bs = bs
combine (a:as) (b:bs)
  | isTransparent a = b : combine as bs
  | otherwise = a : combine as bs

above :: [String] -> [String] -> [String]
above as [] = as
above [] bs = bs
above (a:as) (b:bs) = combine a b : above as bs

below :: [String] -> [String] -> [String]
below a b = above b a
