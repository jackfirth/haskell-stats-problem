module TextStats (
  columnShortestCount,
  mapOverTextColumns,
  ShortestCount
) where

import           Column
import           Data.Maybe

data ShortestCount = ShortestCount String Int | InitialCount deriving Show

columnShortestCount :: DataColumn -> ShortestCount
columnShortestCount = callTextColumn (shortestCount . catMaybes)

shortestCount :: [String] -> ShortestCount
shortestCount = foldr updateShortestCount InitialCount

updateShortestCount :: String -> ShortestCount -> ShortestCount
updateShortestCount s InitialCount = ShortestCount s 1
updateShortestCount s (ShortestCount shortestItem count)
  | s == shortestItem = ShortestCount shortestItem (count + 1)
  | strLess s shortestItem = updateShortestCount s InitialCount
  | otherwise = ShortestCount shortestItem count

strLess :: String -> String -> Bool
strLess s1 s2 = length s1 < length s2 || (length s1 == length s2 && s1 < s2)

mapOverTextColumns :: (DataColumn -> a) -> [DataColumn] -> [a]
mapOverTextColumns f = map f . filter isTextColumn
