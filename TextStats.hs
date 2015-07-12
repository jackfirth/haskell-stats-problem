module TextStats (
  columnShortestCount,
  mapOverTextColumns
) where

import           Column
import           Data.Maybe

data ShortestCountStat = ShortestCountStat Int Int | InitialCount

columnShortestCount :: DataColumn -> Int
columnShortestCount = callTextColumn (shortestCount . catMaybes)

statCount :: ShortestCountStat -> Int
statCount InitialCount = 0
statCount (ShortestCountStat _ count) = count

shortestCount :: [String] -> Int
shortestCount = statCount . foldr updateShortestCount InitialCount

updateShortestCount :: String -> ShortestCountStat -> ShortestCountStat
updateShortestCount s InitialCount = ShortestCountStat (length s) 1
updateShortestCount s (ShortestCountStat shortestLength count)
  | length s < shortestLength = updateShortestCount s InitialCount
  | length s > shortestLength = ShortestCountStat shortestLength count
  | otherwise = ShortestCountStat shortestLength (count + 1)

mapOverTextColumns :: (DataColumn -> a) -> [DataColumn] -> [a]
mapOverTextColumns f = map f . filter isTextColumn
