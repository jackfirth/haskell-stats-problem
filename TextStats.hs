module TextStats (
  columnShortestCount,
  columnLongestCount,
  columnAverageLength,
  mapOverTextColumns,
  ShortestCount,
  LongestCount
) where

import           Average
import           Column
import           Data.List
import           Data.Maybe


mapOverTextColumns :: (DataColumn -> a) -> [DataColumn] -> [a]
mapOverTextColumns f = map f . filter isTextColumn

strLess :: String -> String -> Bool
strLess s1 s2 = length s1 < length s2 || (length s1 == length s2 && s1 < s2)

strGreater :: String -> String -> Bool
strGreater s1 s2 = length s1 > length s2 || (length s1 == length s2 && s1 > s2)


data ShortestCount = ShortestCount String Int | InitialShortestCount deriving Show
data LongestCount = LongestCount String Int | InitialLongestCount deriving Show

columnShortestCount :: DataColumn -> ShortestCount
columnShortestCount = callTextColumn (shortestCount . catMaybes)

columnLongestCount :: DataColumn -> LongestCount
columnLongestCount = callTextColumn (longestCount . catMaybes)


shortestCount :: [String] -> ShortestCount
shortestCount = foldl' (flip updateShortestCount) InitialShortestCount

longestCount :: [String] -> LongestCount
longestCount = foldl' (flip updateLongestCount) InitialLongestCount


updateShortestCount :: String -> ShortestCount -> ShortestCount
updateShortestCount s InitialShortestCount = ShortestCount s 1
updateShortestCount s (ShortestCount shortestItem count)
  | s == shortestItem = ShortestCount shortestItem (count + 1)
  | strLess s shortestItem = updateShortestCount s InitialShortestCount
  | otherwise = ShortestCount shortestItem count

updateLongestCount :: String -> LongestCount -> LongestCount
updateLongestCount s InitialLongestCount = LongestCount s 1
updateLongestCount s (LongestCount longestItem count)
  | s == longestItem = LongestCount longestItem (count + 1)
  | strGreater s longestItem = updateLongestCount s InitialLongestCount
  | otherwise = LongestCount longestItem count


columnAverageLength :: DataColumn -> Float
columnAverageLength = callTextColumn (average . map genericLength . catMaybes)
