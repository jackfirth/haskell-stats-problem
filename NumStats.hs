module NumStats (
  columnMin,
  columnMax,
  columnAverage,
  mapOverNumColumns
) where

import           Column
import           Data.Maybe

columnMin :: DataColumn -> Maybe Float
columnMin = callNumberColumn minimum

columnMax :: DataColumn -> Maybe Float
columnMax = callNumberColumn maximum

columnAverage :: DataColumn -> Maybe Float
columnAverage = callNumberColumn (streamAverage . catMaybes)

mapOverNumColumns :: (DataColumn -> a) -> [DataColumn] -> [a]
mapOverNumColumns f = map f . filter isNumberColumn

streamAverage :: [Float] -> Maybe Float
streamAverage [] = Nothing
streamAverage xs = Just (sum xs / fromIntegral (length xs))
