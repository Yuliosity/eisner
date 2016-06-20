module Language.Algorithms.Eisner.Debug
    (

    ) where

import Text.Printf

import Language.Algorithms.Eisner

testShow2 :: Path -> String
testShow2 (Path {subpathes = Nothing, pathWeight = w}) = show w
testShow2 (Path {subpathes = Just(f, c), pathLink = Just _, pathWeight = w}) =
    printf "%s->%s: %s" (show $ pathIndices f) (show $ pathIndices c) (show w)
testShow2 (Path {subpathes = Just(f, c), pathLink = Nothing, pathWeight = w}) =
    printf "%s+%s: %s" (show $ pathIndices f) (show $ pathIndices c) (show w)

makeTable :: Sentence -> LinkGraph -> PathMatrix -> String
makeTable sentence linkGraph pathMatrix =
    printf "<table border=\"1\">%s</table>" (concatMap makeRow [0 .. len - 1]) where
        len = length sentence
        makeRow :: Int -> String
        makeRow i = printf "  <tr>%s</tr>\n" (concatMap makeCell i [0 .. len - 1])
        makeCell :: Int -> Int -> String
        makeCell i j | i == j    = printf "    <td>%s</td>\n" (show $ sentence !! i)
                     | otherwise = printf "    <td>%s%s</td>\n" (showLink i j) (makeContent i j)
        showLink i j = case M.lookup (sentence !! i, sentence !! j) linkGraph of
                           Nothing -> ""
                           Just (Link {linkWeight = w}) -> printf "%d->%d: %s<br/>" i j (show w)
        makeContent i j = case M.lookup (minI, maxI, dir) pathMatrix of
                              Nothing -> ""
                              Just path -> testShow2 path
                          where
                              minI = min i j
                              maxI = max i j
                              dir = if i <= j then DirRight else DirLeft
