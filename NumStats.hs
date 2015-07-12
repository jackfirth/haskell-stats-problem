module NumStats (
  columnMin,
  columnMax,
  columnAverage,
  mapOverNumColumns
) where

import           Average
import           Column
import           Data.Maybe

maybeAlgebra :: ([a] -> b) -> [a] -> Maybe b
maybeAlgebra _ [] = Nothing
maybeAlgebra f xs = Just (f xs)

columnMin :: DataColumn -> Maybe Float
columnMin = callNumberColumn (maybeAlgebra minimum . catMaybes)

columnMax :: DataColumn -> Maybe Float
columnMax = callNumberColumn (maybeAlgebra maximum . catMaybes)

columnAverage :: DataColumn -> Maybe Float
columnAverage = callNumberColumn (maybeAlgebra average . catMaybes)

mapOverNumColumns :: (DataColumn -> a) -> [DataColumn] -> [a]
mapOverNumColumns f = map f . filter isNumberColumn
