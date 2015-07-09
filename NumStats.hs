module NumStats (
  columnMin,
  columnMax,
  columnAverage
) where

import           Column
import           Data.Maybe

columnMin :: DataColumn -> Maybe Float
columnMin = callNumberColumn minimum

columnMax :: DataColumn -> Maybe Float
columnMax = callNumberColumn maximum

columnAverage :: DataColumn -> Maybe Float
columnAverage = callNumberColumn (streamAverage . catMaybes)

streamAverage :: [Float] -> Maybe Float
streamAverage [] = Nothing
streamAverage xs = Just (sum xs / fromIntegral (length xs))
